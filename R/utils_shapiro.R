# Utils_shapiro.R
#shapiro-wilk
# Dynamically render the heading based on the checkbox
render_shapiroHeading <- function(input){
  renderUI({
    if (!is.null(input$normality_test) && any(input$normality_test %in% c("shapiro", "ks", "qqplot", "density"))) {      
      h4(HTML("<b>Normality Test</b>"))  # Display the heading
    }
  })
}

# reactive dataset for selected samples
filter_data_stats <- function(input, stats_data){
  reactive({
    req(input$select_dcq_or_ddcq_stats, input$sampleInput, input$columnInput, stats_data()) 
    # Initial data filtering and selection
    data <- stats_data() %>%
      filter(cell %in% input$sampleInput) %>%
      dplyr::select(cell, !!as.symbol(input$columnInput)) %>%
      filter(!is.na(!!as.symbol(input$columnInput))) %>%
      droplevels()
    # Apply log10 transformation if the checkbox is checked
    if (input$log_transform == TRUE) {
      data <- data %>%
        mutate(!!as.symbol(input$columnInput) := log10(!!as.symbol(input$columnInput)))
    }
    data
  }) 
}

# Reactive expression for performing the Shapiro-Wilk test
perform_shapiro <- function(input, shapiro_data_reactive){
  reactive({
    req(input$normality_test == "shapiro") # Proceed only if Shapiro-Wilk is selected
    data_for_shapiro <- shapiro_data_reactive() # Use the reactive filtered data
    
    # Split the data by the 'cell' factor and perform Shapiro-Wilk test for each group
    shapiro_col <- input$columnInput
    grouped_data <- split(data_for_shapiro[[shapiro_col]], data_for_shapiro$cell)
    
    # Initialize an empty data frame to store the results
    results_df <- data.frame(
      Group = character(),
      W = numeric(),
      P_value = numeric(),
      Passed_normality_test = logical(),
      P_value_summary = character(),
      stringsAsFactors = FALSE
    )
    
    # Loop through the list of groups and perform the Shapiro-Wilk test
    for (group_name in names(grouped_data)) {
      test_result <- tryCatch({
        shapiro.test(grouped_data[[group_name]])
      }, error = function(e) {
        return(list(statistic = NA, p.value = NA))
      })
      
      # Determine if the group passes the normality test based on the p-value
      passed_test <- test_result$p.value > 0.05
      # Determine the p-value summary
      p_value_summary <- ifelse(test_result$p.value < 0.001, "***", 
                                ifelse(test_result$p.value < 0.01, "**", 
                                       ifelse(test_result$p.value > 0.05, "ns", "*")))
      
      # Add the results to the data frame
      results_df <- rbind(results_df, data.frame(
        Group = group_name,
        W = test_result$statistic,
        P_value = test_result$p.value,
        Passed_normality_test = ifelse(passed_test, "Yes", "No"),
        P_value_summary = p_value_summary,
        stringsAsFactors = FALSE
      ))
      
    }
    rownames(results_df) <- NULL
    # Return the results data frame
    return(results_df)
  })
}

# shapiro test and display table.
shapiro_results <- function(input, output, test_results_shapiro) {
  observe({
    results_shapiro <- test_results_shapiro() # Assumes this reactive expression exists
    
    results_shapiro <- results_shapiro %>% 
      rename("P Value" = P_value, "Passed Normality Test?" = Passed_normality_test, "P Value Summary" = P_value_summary)
    # Dynamically create or remove the table based on selection
    output$normalityTableUI <- renderUI({
      if("shapiro" %in% input$normality_test) {
        # Check if there's an error in the results and display it, otherwise display the table
        if (is.list(results_shapiro) && !is.null(results_shapiro$error)) {
          # Output the error message if present
          tableOutput("shapiroError")
        } else {
          #Construct and render the results table
          dataTableOutput("normalityTable")
        }
      }
    })
    
    # Conditionally render the error message or the results table
    output$shapiroError <- renderTable({
      if(is.list(results_shapiro) && !is.null(results_shapiro$error)) {
        matrix(results_shapiro$error, nrow = 1)
      }
    })
    
    output$normalityTable <- renderDataTable({
      if(!is.null(results_shapiro) && is.list(results_shapiro) && is.null(results_shapiro$error)) {
        results_shapiro
      }
    })
  })
}