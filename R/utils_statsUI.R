#utils_statsUI.R
displayGeneUI <- function(input){
  # Check if 'ddcq' is selected
  if (input$select_dcq_or_ddcq_stats == "ddcq_stats") {
    # Display the gene selection message
    output$selected_gene_ui_stats <- renderUI({
      textOutput("selected_gene_message_stats")
    })
  } else {
    # Hide the message when 'dcq' is selected or for any other condition
    output$selected_gene_ui_stats <- renderUI({})
  }
}

# Define the text output for displaying the selected gene
displayGeneUI_message <- function(input){
  renderText({
    req(input$select_gene)  # Ensure there is a selection
    # Use gsub to remove "dcq_" from the selected gene's name
    selected_gene_cleaned <- gsub("^dcq_", "", input$select_gene)
    paste("You are currently performing stats on gene:", selected_gene_cleaned)
  })
}


display_statsSamplesUI <- function(input, session, values, wrangled_data, average_dcq){
  if (input$select_dcq_or_ddcq_stats == "dcq_stats") {
    updateSelectInput(session, "sampleInput", choices = unique(wrangled_data()$cell))
    updateSelectInput(session, "columnInput", choices = grep("^fc_dcq_", names(wrangled_data()), value = TRUE))
  } else if (input$select_dcq_or_ddcq_stats == "ddcq_stats") {
    # Check if avg_dcq_df is not NULL and has the expected columns
    if (!is.null(values$ddcqDataSaved) && values$ddcqDataSaved) {
      updateSelectInput(session, "sampleInput", choices = unique(average_dcq()$cell))
      updateSelectInput(session, "columnInput", choices = grep("^fc_ddcq", names(average_dcq()), value = TRUE))
    } else {
      updateSelectInput(session, "sampleInput", choices=character(0), selected = character(0))  # Clear the sampleInput choices
      updateSelectInput(session, "columnInput", choices=character(0), selected = character(0))
    }
  }
}

ddcq_not_calculated_msg <- function(input, values){
  renderUI({
    # Check if 'ddcq_stats' is selected and data is not saved yet
    if (input$select_dcq_or_ddcq_stats == "ddcq_stats" && !values$ddcqDataSaved) {
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
}

# Define the reactive expression for the dataframe to be used in the stats tab
choose_stats_df <- function(input, values, wrangled_data, average_dcq){
  reactive({
    if (input$select_dcq_or_ddcq_stats == "dcq_stats") {
      wrangled_data()
    } else if (input$select_dcq_or_ddcq_stats == "ddcq_stats" && values$ddcqDataSaved) {
      average_dcq()
    } else {
      return(NULL)
    }
  })
}
 