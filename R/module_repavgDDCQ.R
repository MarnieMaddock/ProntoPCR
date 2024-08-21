# module_repavgDDCQ.R
DDCQrepMain <- function(id){
  ns <- NS(id)
  tagList(
    h4(HTML("<b>Biological Replicate Average Values</b>")), 
    dataTableOutput(ns("rep_avg_table_ddcq")), #display biological replicate values for ddcq
    downloadUI(ns("download_ddcq_avg_data"), "Download Replicate Data") #download as a csv
    ) 
}

DDCQrepServer <- function(id, average_dcq, selected_gene){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rep_avg_data_ddcq <- reactive({
        req(average_dcq())

        rep_avg_ddcq <- average_dcq() %>%
          group_by(cell) %>%
          #summarize using geometric mean
          summarize(mean_fc_ddcq = exp(mean(log(fc_ddcq), na.rm = TRUE)), .groups = "drop")
        
        return(rep_avg_ddcq)
      })
    
    # Display the replicate averages table in "Calculations" tab
    output$rep_avg_table_ddcq <- renderDataTable({
        rep_avg_data_ddcq()
      }, options = list(pageLength = 5))
    

    
    #download button to save replicate values
    downloadServer("download_ddcq_avg_data", rep_avg_data_ddcq, function(input, session) {
      paste("DDCq_processed_replicate_data_", selected_gene(), "_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
    })
    
    return(list(
      rep_avg_data_ddcq = rep_avg_data_ddcq
    ))
    
  })
}