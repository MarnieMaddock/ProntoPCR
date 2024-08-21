#module_deltadeltaCq.R
#
ddcqSidebar <- function(id){
  ns <- NS(id)
  tagList(
    h6("Ensure that each sample has all housekeeper gene measurements (i.e. there are no missing values) to avoid having inaccurate data points."),
    tags$br(),
    helpText(HTML("dcq_ctrl_avg: Calculates the average of the control group for the selected gene.")),
    tags$br(),
    helpText(HTML("ddcq_gene: Calculates the difference between the ΔCq of your gene and the average of the control group i.e. ΔΔCq = ΔCq (gene of interest) – ΔCq (control group).")),
    tags$br(),
    helpText(HTML("fc_ddcq: Fold change of the ΔΔCq value i.e. 2<sup>-(ΔΔCq)</sup>.")),
    tags$br(),
    tags$br(),
    fluidRow(
      column(width = 5, uiOutput(ns("select_control"))), # select control group
      column(width = 5, uiOutput(ns("select_samples"))), # select the rest of the samples to be analyzed
      column(width = 5, uiOutput(ns("column_selector2")))), # select the gene
    actionButton(ns("save_ddcq_data"), "Save ΔΔCq Data")
  )
}

ddcqMain <- function(id){
  ns <- NS(id)
  tagList(
    h4(HTML("<b>Average ∆Cq for control and perform 2<sup>-∆ΔCq</sup></b>")),
    dataTableOutput(ns("ddcq_data")), #display processed ddcq data
    downloadUI(ns("download_ddcq_data"), "Download Processed Data"), #download ddcq data as a csv
    tags$br()
  )
}

ddcqServer <- function(id, wrangled_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$select_control <- renderUI({
      req(wrangled_data())
      selectInput(ns("select_control"), "Select the control/untreated sample", choices = unique(wrangled_data()$cell))
    })
    
    output$select_samples <- renderUI({
      req(wrangled_data())
      selectInput(ns("select_samples"), "Select the diseased/treated sample(s)", choices = unique(wrangled_data()$cell), multiple = T)
    })
    
    output$column_selector2 <- renderUI({
      req(wrangled_data())
      
      # Filter column names to include only those starting with "dcq"
      dcq_columns <- grep("^dcq_", colnames(wrangled_data()), value = TRUE)
      
      # Generate selectInput for choosing the column dynamically
      selectInput(ns("select_gene"), "Select Gene to calculate ΔΔCq", choices = dcq_columns)
    })
    
    extracted_gene <- reactive({
      selected_gene <- input$select_gene
      str_extract(selected_gene, "(?<=_).*")
    })
    
    # create ddcq data
    ddcq_filtered_data <- reactive({
        req(wrangled_data())
        req(input$select_gene)

        control <- input$select_control
        samples <- input$select_samples
        selected_gene <- input$select_gene
        
        #select for required samples and keep relevant columns
        ddcq_data <- wrangled_data() %>%
          filter((cell == control) | (cell %in% samples)) %>%
          dplyr::select(Sample, condition, group, cell, mean_hk, extracted_gene(), all_of(selected_gene))

        # Resetting levels of factors to only include selected options
        ddcq_data$cell <- factor(ddcq_data$cell, levels = unique(c(as.character(control), as.character(samples))))
        return(ddcq_data)
      })
    
    # Calculate the average delta cq for the selected gene in the control samples
    average_dcq <- reactive({
        req(wrangled_data())
        req(input$select_gene)
        req(input$select_control)

        selected_gene <- input$select_gene
        control <- input$select_control
        samples <- input$select_samples
        mean_value <- reactiveVal(NULL)

        # Calculate the average delta cq for the selected gene in the control samples
        avg_dcq_ctrl <- ddcq_filtered_data() %>%
          filter(cell == control) %>%
          group_by(cell) %>%
          summarise(dcq_ctrl_avg = mean(!!sym(selected_gene), na.rm = TRUE), .groups = "drop")

        # Left join the original dataframe with the summarised dataframe
        avg_dcq_ctrl <- left_join(ddcq_filtered_data(), avg_dcq_ctrl, by = "cell")

        # Calculate the mean value
        mean_val <- mean(avg_dcq_ctrl$dcq_ctrl_avg, na.rm = TRUE)

        # Store the mean value in the reactive value
        mean_value(mean_val)

        # Assign the mean value to the entire dcq_ctrl_avg column
        avg_dcq_ctrl$dcq_ctrl_avg <- mean_val

        # Create a new column ddcq by subtracting selected_gene from dcq_ctrl_avg
        avg_dcq_ctrl$ddcq <-  avg_dcq_ctrl[[selected_gene]] - avg_dcq_ctrl$dcq_ctrl_avg

        # Create a new column fc_ddcq containing 2^(-ddcq)
        avg_dcq_ctrl$fc_ddcq <- 2^(-avg_dcq_ctrl$ddcq)

        # Resetting levels of factors to only include selected options
        avg_dcq_ctrl$cell <- factor(avg_dcq_ctrl$cell, levels = unique(c(as.character(control), as.character(samples))))

        return(avg_dcq_ctrl)
      })
    
    #display ddcq data in UI
    output$ddcq_data <- renderDataTable({
        req(average_dcq())
        average_dcq()
      }, options = list(pageLength = 5))

    #save the condition to use in string for saving csv file
    gene_for_download <- reactive({
      input$select_gene
    })
    
    downloadServer("download_ddcq_data", average_dcq, function(input, session) {
      paste("DDCQ_processed_data_", gene_for_download(), "_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
    })

    values <- reactiveValues(ddcqDataSaved = FALSE)
    observeEvent(input$save_ddcq_data, {
      # This block is executed whenever the 'Save ddcq Data' button is clicked.
      # Even though you don't want to perform any action immediately when the button is clicked,
      # use this as a trigger for other reactive expressions or observers.
      values$ddcqDataSaved <- TRUE
    })
    
    return(list(
      average_dcq = average_dcq,
      extracted_gene = extracted_gene,
      values = values,
      gene_for_download = gene_for_download
    ))
  })
}

    
