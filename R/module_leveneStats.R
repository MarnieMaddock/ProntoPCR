#module_leveneStats.R

leveneSidebar <- function(id) {
  ns <- NS(id)
  tagList(
    h6(HTML("<b>3. Homogeneity of Variance:</b>")),
    checkboxInput(ns("variance"), "Levene's test", value = FALSE),
    
  )
}

leveneMain <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("leveneHeading")), # display leven heading if this is selected in the sidebar panel
    uiOutput(ns("leveneUI")), # display levene test results if selected
  )
}

leveneServer <- function(id, columnInput, stats_data, selected_stat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # create heading if variance box is clicked in sidepanel
    output$leveneHeading  <- renderUI({
        if (input$variance == TRUE ) {      
          h4(HTML("<b>Homogeneity of Variance Test</b>"))  # Display the heading
        }
      })

    # calculate and display homogenity of variance levenes results
    levene_reactive <- reactive({
        req(input$variance == TRUE, columnInput())
        levene_test_data <- stats_data()
        test_result <- leveneTest(levene_test_data[[columnInput()]] ~ levene_test_data$cell)
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
          P_value_summary = ifelse(p_value <= 0.001, "***", 
                                   ifelse(p_value <= 0.01, "**", 
                                          ifelse(p_value > 0.05, "ns", "*")))
        )
        summary_df <- summary_df %>% 
          rename("df (Group)" = DF_Group, "df (Error)" = DF_Error, "F" = F_Value, 
                 "P Value" = P_Value, "Passed Variance Test?" = Passed_variance_test, "P Value Summary" = P_value_summary)
        rownames(summary_df) <- ""
        # Return the new summary data frame
        summary_df
      })
    
    # display levene's test
    output$levene  <- renderDataTable({
        req(levene_reactive())
        levene_reactive()
      })
    
    output$leveneUI <- renderUI({
        if(input$variance == TRUE) { # Check if the user wants to see the Levene's test results
          dataTableOutput(ns("levene"))
        }
      })
    
    observeEvent(selected_stat(), {
      updateCheckboxInput(session, "variance", value = FALSE)
    })
    
    leveneResults <- reactive({
      if (input$variance == TRUE) {
        levene_reactive() 
      } else {
        NULL
      }
    })
    
    levene_input <- reactive({
      input$variance
    })
    
    return(list(
      leveneResults = leveneResults,
      levene_input = levene_input
    ))
  })
}