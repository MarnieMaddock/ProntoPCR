#module_repavg

repDataUI <- function(id){
  ns <- NS(id)
  tagList(
    h4(HTML("<b>Biological Replicate Average Values</b>")),
    tags$br(),
    dataTableOutput(ns("rep_avg_table")),
    downloadUI(ns("download_rep_avg_data"), "Download Replicate Average Data"), #download as csv
    tags$br(),
    tags$br(),
    h4(HTML("<b>Filter by Condition</b>")),
    dataTableOutput(ns("rep_avg_filtered_table")),
    downloadUI(ns("download_rep_avg_filtered_data"), "Download Filtered Replicate Average Data"),  #download as csv
    tags$br(),
    tags$br()
  )
}

repDataServer <- function(id, data, filter_condition){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    #replicate averages datasets + calcs
    # Calculate replicate averages when data is loaded
    rep_avg_data  <- reactive({
      req(data())
      
      vars <- colnames(data()) %>%
        grep("^fc_dcq", ., value = TRUE)
      

      
      # Add and relocate 'cell' column
      rep_avg <- data() %>%
        group_by(condition, group) %>%
        summarise_at(vars, list(fc_avg = ~mean(., na.rm = TRUE))) %>%
        gather(key = "Variable", value = "fc_avg", -condition, -group)

      rep_avg <- rep_avg %>%
        pivot_wider(names_from = Variable, values_from = fc_avg)
      
      # Remove "_fc_avg" from column names
      colnames(rep_avg) <- sub("_fc_avg$", "", colnames(rep_avg))
      
      #add column cell
      rep_avg$cell <- paste(rep_avg$condition, rep_avg$group, sep = "_")
      rep_avg$cell <- as.factor(rep_avg$cell)
      #Move column
      rep_avg <- rep_avg %>%
        relocate(cell, .after = group)
      #rep_avg <- datawizard::data_relocate(rep_avg, select = "cell", after = "group")
      

      #add standard deviation, Standard error and confiencde interval

      
      return(rep_avg)
    })
    
    # Display the replicate averages table in "Calculations" tab
    output$rep_avg_table <- renderDataTable({
      req(rep_avg_data())
      rep_avg_data()
    }, options = list(pageLength = 5, scrollX = TRUE, scrollY = "200px"))
    
    filtered_rep_avg_dataset <- reactive({
      req(rep_avg_data(), filter_condition())
      conditions_to_filter <- filter_condition()
      
      if (!is.null(conditions_to_filter)) {
        filtered_data <- rep_avg_data() %>%
          filter(condition %in% conditions_to_filter)
        return(filtered_data)
      } else {
        return(NULL)
      }
    })
    
    # Display filtered replicate data table in the UI
    output$rep_avg_filtered_table <- renderDataTable({
      req(filtered_rep_avg_dataset())
      filtered_rep_avg_dataset()
    }, options = list(pageLength = 5, scrollX = TRUE, scrollY = "200px"))

    # Download filtered replicate data as csv
    # download replicate data
    downloadServer("download_rep_avg_data", rep_avg_data, function(input, session) {
      paste("Replicate_avg_data_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
    })
    
    
    #download replicate average filtered dataset
    downloadServer("download_rep_avg_filtered_data", filtered_rep_avg_dataset, function(input, session) {
      condition <- filter_condition()  # Directly use input$condition
      if (!is.null(condition)) {
        paste("Replicate_avg_data_", condition, "_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
      } else {
        paste("Replicate_avg_data_filtered_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
      }
    })
    
    return(list(
      rep_avg_data = rep_avg_data,
      filtered_rep_avg_dataset = filtered_rep_avg_dataset
    ))
  })
}



