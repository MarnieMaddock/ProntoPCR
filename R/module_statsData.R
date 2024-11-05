# module_statsData.R

statsDataSidebar <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("select_dcq_or_ddcq_stats"), 
                 HTML("Select whether to use 2<sup>-(ΔCq)</sup>, 2<sup>-(ΔΔCq)</sup> or the mean of the housekeepers for statistical tests:"),
                 choices = c("ΔCq" = "dcq_stats", "∆ΔCq" = "ddcq_stats", "Housekeeper Mean" = "housekeeper_stats"),
                 selected = "dcq_stats"),
    h6("Select the samples and gene to perform statistics on."),
    uiOutput(ns("ddcqMessage")), #displays error message if ddcq stats has not been performed/saved yet.
    selectInput(ns("sampleInput"), "Select Samples:", choices = NULL, multiple = TRUE), #select samples
    uiOutput(ns("selected_gene_ui_stats")), #displays "You are currently performing stats on gene: if ddcq is selected"
    tags$br(),
    selectInput(ns("columnInput"), "Select Gene:", choices = NULL), #select gene if dcq is selected)
  )
}


statsDataServer <- function(id, values, dcq_data, ddcq_data, ddcq_selected_gene, log_transform) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #save selected option i.e. DCQ or DDCQ for stats
    selected_stat <- reactive({
      input$select_dcq_or_ddcq_stats
    })
    
    # Define the text output for displaying the selected gene
    output$selected_gene_message_stats <- renderUI({
        req(ddcq_selected_gene())  # Ensure there is a selection
        # Use gsub to remove "dcq_" from the selected gene's name
        selected_gene_cleaned <- gsub("^dcq_", "", ddcq_selected_gene())
        HTML(paste0("You are currently performing stats on gene: ", selected_gene_cleaned, ". To change the gene, update the ΔΔCq dataset in the 2<sup>-(ΔΔCq)</sup> tab. "))
      })
    
    # Conditionally render the UI element based on the selected stat
    output$selected_gene_ui_stats <- renderUI({
      if (selected_stat() == "ddcq_stats") {
        uiOutput(ns("selected_gene_message_stats"))
      } else {
        NULL
      }
    })
    
    #error messgae if DDCQ calculations havent occurred yet
    output$ddcqMessage <-  renderUI({
        req(selected_stat())  # Ensure there is a selection
        # Check if 'ddcq_stats' is selected and data is not saved yet
        if (selected_stat() == "ddcq_stats" && !values$ddcqDataSaved) {
          # Return a UI element with the message
          tagList(
            HTML('<h5>Please go to the 2<sup>-(∆∆Cq)</sup> Calculations tab to create your ∆∆Cq dataset.</h5>'),
            tags$p("You need to save your ∆∆Cq dataset before proceeding.")
          )
        } else {
          # Return NULL or an empty UI element if conditions are not met
          return()
        }
      })
    
    observe({
      req(selected_stat())
      # Use values, dcq_data, and ddcq_data for some logic
      if (selected_stat() == "dcq_stats") {
        updateSelectInput(session, "sampleInput", choices = unique(dcq_data()$cell))
        updateSelectInput(session, "columnInput", choices = grep("^fc_dcq_", names(dcq_data()), value = TRUE))
      } else if (selected_stat() == "ddcq_stats") {
        # Check if ddcq_data is not NULL and has the expected columns
        if (!is.null(values$ddcqDataSaved) && values$ddcqDataSaved) {
          updateSelectInput(session, "sampleInput", choices = unique(ddcq_data()$cell))
          updateSelectInput(session, "columnInput", choices = grep("^fc_ddcq", names(ddcq_data()), value = TRUE))
        } else {
          updateSelectInput(session, "sampleInput", choices=character(0), selected = character(0))  # Clear the sampleInput choices
          updateSelectInput(session, "columnInput", choices=character(0), selected = character(0))
        }
      } else if (selected_stat() == "housekeeper_stats"){
        updateSelectInput(session, "sampleInput", choices= unique(dcq_data()$cell))  # Clear the sampleInput choices
        updateSelectInput(session, "columnInput", choices= "mean_hk")
      } else {
        updateSelectInput(session, "sampleInput", choices=character(0), selected = character(0))  # Clear the sampleInput choices
        updateSelectInput(session, "columnInput", choices=character(0), selected = character(0))
      }
    })
    
    
    #Choose data
    stats_data <- reactive({
      if (selected_stat() == "dcq_stats") {
        dcq_data()
      } else if (selected_stat() == "ddcq_stats" && values$ddcqDataSaved) {
        ddcq_data()
      } else if (selected_stat() == "housekeeper_stats") {
        dcq_data() %>% 
          dplyr::select(Sample, condition, group, cell, mean_hk)
      } else {
        return(NULL)
      }
    })
    
    #save sampleInput
    sampleInput <- reactive({
      input$sampleInput
    })
    
    #save gene selected
    columnInput <- reactive({
      input$columnInput
    })
    

    # Reactive dataset for selected samples
    filter_data_stats <- reactive({
      req(selected_stat(), sampleInput(), columnInput(), stats_data()) 
      
      # Initial data filtering and selection
      data <- stats_data()
      
      # Check if the column exists in the dataset
      validate(
        need(columnInput() %in% colnames(data), paste("Column", columnInput(), "If this error persists, contact the developer."))
      )
      
      data <- data %>%
        dplyr::filter(cell %in% sampleInput()) %>%
        dplyr::select(cell, !!as.symbol(columnInput())) %>%
        dplyr::filter(!is.na(!!as.symbol(columnInput()))) %>%
        droplevels()
      
      # Apply log10 transformation if the checkbox is checked
      if (!is.null(log_transform()) && length(log_transform()) > 0) {
        if (log_transform() == "Log10") {
          data <- data %>%
            dplyr::mutate(!!as.symbol(columnInput()) := log10(!!as.symbol(columnInput())))
        } else if (log_transform() == "Log2") {
          data <- data %>%
            dplyr::mutate(!!as.symbol(columnInput()) := log2(!!as.symbol(columnInput())))
        }
      } else {
        data <- data
      }
      data
    })
    
    return(list(
      stats_data = stats_data,
      sampleInput = sampleInput,
      columnInput = columnInput,
      filter_data_stats = filter_data_stats,
      selected_stat = selected_stat
    ))
  })
}
