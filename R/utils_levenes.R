# utils_levenes.R

# create heading if variance box is clicked in sidepanel
render_leveneHeading <- function(input){
  renderUI({
    if (input$variance == TRUE ) {      
      h4(HTML("<b>Homogeneity of Variance Test</b>"))  # Display the heading
    }
  })
}

# calculate and display homogenity of variance levenes results
calc_levenes <- function(input, shapiro_data_reactive){
  reactive({
    req(input$variance == TRUE, input$columnInput)
    levene_test_data <- shapiro_data_reactive()
    test_result <- leveneTest(levene_test_data[[input$columnInput]] ~ levene_test_data$cell)
    # Extracting values from the test_result
    df_group <- test_result$Df[1] # Degrees of freedom for group
    df_error <- test_result$Df[2] # Degrees of freedom for error/residuals
    f_value <- test_result$`F value`[1] # Correct access for F value
    p_value <- test_result$`Pr(>F)`[1]
    # Construct summary data frame
    summary_df <- data.frame(
      DF_Group = df_group,
      DF_Error = df_error,
      F_Value = f_value,
      P_Value = p_value,
      Passed_variance_test = ifelse(p_value > 0.05, "Yes", "No"),
      P_value_summary = ifelse(p_value < 0.001, "***", 
                               ifelse(p_value < 0.01, "**", 
                                      ifelse(p_value > 0.05, "ns", "*")))
    )
    summary_df <- summary_df %>% 
      rename("df (Group)" = DF_Group, "df (Error)" = DF_Error, "F" = F_Value, 
             "P Value" = P_Value, "Passed Variance Test?" = Passed_variance_test, "P Value Summary" = P_value_summary)
    rownames(summary_df) <- ""
    # Return the new summary data frame
    summary_df
  })
}


# render levene test
levene_table <- function(levene_reactive){
  renderDataTable({
    req(levene_reactive())
    levene_reactive()
  })
}

render_leveneUI <- function(input, levene){
  renderUI({
    if(input$variance == TRUE) { # Check if the user wants to see the Levene's test results
      dataTableOutput("levene")
    }
  })
}