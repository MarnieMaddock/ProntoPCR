#module_deltaCq.R
library(shiny)
library(dplyr)

wrangleDataSidebar <- function(id){
  ns <- NS(id)
  tagList(
    h6("Ensure that each sample has all housekeeper gene measurements (i.e. there are no missing values) to avoid having inaccurate data points."),
    tags$br(),
    helpText("mean_hk: Calculates the mean of the houskeepers you specified in the input data tab."),
    tags$br(),
    helpText("dcq_gene: Calculates the difference between the Cq value of your gene and the average of your housekeepers i.e. ΔCq = Cq (gene of interest) – Cq (average of the housekeeping genes)."),
    tags$br(),
    helpText(HTML("fc_dcq_gene: Calculates the relative mRNA fold change - and is useful when you don't have an appropriate control/untreated reference value i.e. 2<sup>-(ΔCq)</sup>.")),
    tags$br(),
    checkboxInput(ns("geo_mean"), 
                  label = tags$span("Use geometric mean for averaging housekeeping genes?", 
                                    tags$i(
                                      class = "glyphicon glyphicon-info-sign", 
                                      style = "color:#00359bff;", 
                                      title = "Please check the box to use the geometric mean for averaging housekeeping genes. By default, the arithmetic mean is used."
                                    )
                  )
    ),
    tags$br(),
    tags$br(),
    uiOutput(ns("condition_filter"))
  )
}
wrangleDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4(HTML("<b>Average the houskeeping genes and perform ∆Cq and 2<sup>-∆Cq</sup></b>")),
    tags$br(),
    DT::DTOutput(ns("wrangled_table")),
    downloadUI(ns("download_processed_data"), "Download Processed Data"), #download dcq data as csv
    tags$br(),
    tags$br(),
    h4(HTML("<b>Filter by Condition</b>")),
    DT::DTOutput(ns("filtered_table")),
    downloadUI(ns("download_filtered_data"), "Download Filtered Data") #download filtered data as csv
  )
}


wrangleDataServer <- function(id, save_btn, data, saved_variables) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to store the state of geo_mean
    geo_mean_state <- reactiveVal(FALSE)
    
    observeEvent(input$geo_mean, {
      geo_mean_state(input$geo_mean)
    })
    
    wrangled_data <- reactive({
      req(save_btn())
      df <- data()

      if (is.null(df)) {
        return(NULL)
      }
      # Remove Cq.SD and Quality issues columns
      df <- df[, c(1, 2, 4)]

      # Check if 'NTC' is present in the 'Sample' column and remove if present
      df <- df[!grepl('NTC', df$Sample), ]

      # Check if 'NTC' is present in the 'Target' column and remove if present
      df <- df[!grepl('NTC', df$Target), ]

      #make the dataframe longer (with targets as columns)
      df <- df[,1:3] %>% 
        tidyr::pivot_wider(names_from = Target, values_from = Cq.Mean)
      
      # Retrieve the saved variables
      variables <- saved_variables$names
      # Check if all provided housekeeper names exist as columns
      missing_columns <- setdiff(variables, names(df))
      if (length(missing_columns) > 0) {
        # Show an error modal if there are missing columns
        shiny::showModal(modalDialog(
          title = "Error",
          paste("The following housekeeper names are not found:", paste(missing_columns, collapse = ", ")),
          ". Please check the spelling or capitalisations of these genes in your inserted data sheet and try again.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return(NULL)  # Return NULL to prevent further processing
      }
      
      #Average the housekeepers
      df <- df %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          mean_hk = if (isTRUE(geo_mean_state())) {
            # Calculate geometric mean if checkbox is selected
            exp(mean(log(c_across(all_of(variables))), na.rm = TRUE)) 
          } else { # use arithmetic mean
            mean(c_across(all_of(variables)), na.rm = TRUE)
          }
        )
      #move columns using datawizard package
      df <- datawizard::data_relocate(df, select = "mean_hk", after = "Sample")
      df <- datawizard::data_relocate(df, select = saved_variables$names, after = "Sample")
      
      #Calculate delta Cq for each target
      #which(names(data) == "mean_hk") finds the column index of "mean_hk" in the dataframe.
      #+ 1 increments the index to select the columns directly after "mean_hk".
      #ncol(data) provides the last column index of the dataframe.
      df <- df %>%
        mutate(across((which(names(df) == "mean_hk") + 1):ncol(df),
                      list(dcq = ~ ifelse(.x != 0, .x - mean_hk, 0)),
                      .names = "{.fn}_{.col}"))
      
      #calulcate fold change (relative mRNA)
      # Calculate fc, considering the case where the data point is 0
      #supressed warnings as it is inconsequential to the data functionality
      df <- suppressWarnings({df %>%
          mutate(across(
            (which(startsWith(names(df), "dcq_"))):ncol(df),
            list(fc = ~ ifelse(.x != 0, 2^(-.x), 0)),
            .names = "{.fn}_{.col}"
          ))
      })
      
      # Make a new column that places each sample as the specified condition
      #regex extracts characters after the last underscore
      df$condition <- gsub(".*_(\\w+)$", "\\1", df$Sample)
      df$condition <- as.factor(df$condition)
      #Add group data
      #regex extracts characters before the first underscore
      df$group <- gsub("^([^_]+)_.*$", "\\1", df$Sample)
      df$group <- as.factor(df$group)
      
      #add combined
      df$cell <- paste(df$condition, df$group, sep = "_")
      df$cell <- as.factor(df$cell)
      #Move column using datawizard package
      df <- datawizard::data_relocate(df, select = "group", after = "Sample")
      df <- datawizard::data_relocate(df, select = "condition", after = "Sample")
      df <- datawizard::data_relocate(df, select = "cell", after = "group")
      return(df)
    })
    
    output$wrangled_table <- DT::renderDT({
      req(wrangled_data())
      wrangled_data()
    })
    
    # Render the UI for filtering by condition
    output$condition_filter <- renderUI({
      req(wrangled_data())
      conditions <- unique(wrangled_data()$condition)
      selectInput(ns("condition"), "Select Condition", choices = conditions)
    })
    
    filter_condition <- reactive({
      req(input$condition)
      input$condition
    })
    
    # Create filtered dataset
    filtered_data <- reactive({
      req(wrangled_data())
      conditions_to_filter <- input$condition
      
      if (!is.null(conditions_to_filter)) {
        filtered_data <- wrangled_data() %>%
          filter(condition %in% conditions_to_filter)
        return(filtered_data)
      } else {
        return(NULL)
      }
    })
    
    # Render the filtered data table
    output$filtered_table <- DT::renderDT({
      req(filtered_data())
      filtered_data()
    }, options = list(pageLength = 5))
    
    #download processed dcq data as a csv.
    downloadServer("download_processed_data", wrangled_data, function(input, session) {
      paste("processed_PCR_DCQ_data_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
    })
    
    # download filtered data
    downloadServer("download_filtered_data", filtered_data, function(input, session) {
      if (!is.null(filter_condition())) {
        paste("filtered_PCR_DCQ_data_", filter_condition(), "_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
      } else {
        paste("filtered_PCR_DCQ_data_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
      }
    })
    
    return(list(
      wrangled_data = wrangled_data,
      filter_condition = filter_condition
    ))
  })
}



