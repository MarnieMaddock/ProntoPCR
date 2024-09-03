#source("utils_graphTheme.R")

normalitySidebar <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxGroupInput(ns("normality_test"), 
                       label = HTML(
                         paste0(
                          "<b>2. Normality tests:  </b>",
                              tags$i(
                                class = "glyphicon glyphicon-info-sign", 
                                style = "color:#00359bff;", 
                                title = "Note: Normality testing is usually considered to be unreliable for small sample sizes. If one-way ANOVA is selected, the normality tests will be performed on the residuals, not the raw data. The raw values are the default if no comparison of groups test, or any other group comparison tests are selected."
                              ) 
                          )
                        ), 
                       choices = c("Shapiro-Wilk" = "shapiro", "QQ-Plot" = "qqplot", "Density Plot" = "density"),
                       selected = NULL),
    
    )
}



normalityMain <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("normalityHeading")), # display heading if normality option is selected in the sidepanel
    uiOutput(ns("normalityTableUI")), # display the output for the selected normality option
    uiOutput(ns("residualsFitPlotUI")), # display residuals vs. fitted plot if selected
    uiOutput(ns("qqPlotUI")), # display qq plot if selected
    uiOutput(ns("densityPlotUI")), # display density plot if selected
  )
}


normalityServer <- function(id, sampleInput, columnInput, stats_data, theme_Marnie, test_results, group_comparison, selected_stat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Dynamically render the heading based on the checkbox
    output$normalityHeading <- renderUI({
        if (!is.null(input$normality_test) && any(input$normality_test %in% c("shapiro", "ks", "qqplot", "density"))) {      
          h4(HTML("<b>Normality Testing</b>"))  # Display the heading
        }
      })
    
    num_groups <- reactive({
      req(stats_data())
      length(unique(stats_data()$cell))
    })
    
    #shapiro test for residuals after ANOVA
    residuals_sw <- reactive({
    req(num_groups() > 2, group_comparison() == "parametric")
    # Perform one-way ANOVA
    formula_str <- paste(columnInput(), "~ cell")
    aov_formula <- as.formula(formula_str)
    aov_result <- stats::aov(aov_formula, data = stats_data())
        
        # Calculate residuals for normality testing
        residual_values <- residuals(aov_result)
        sw_res <- stats::shapiro.test(residual_values)
        # Return Shapiro-Wilk result
        # Create a data frame with results similar to the raw data format
        results_df <- data.frame(
          Group = "Residuals",
          W = sw_res$statistic,
          P_value = sw_res$p.value,
          Passed_normality_test = ifelse(sw_res$p.value > 0.05, "Yes", "No"),
          P_value_summary = ifelse(sw_res$p.value <= 0.001, "***",
                                   ifelse(sw_res$p.value <= 0.01, "**",
                                          ifelse(sw_res$p.value <= 0.05, "*", "ns"))),
          stringsAsFactors = FALSE
        )
        
        return(results_df)
    })
    
    res_fit_plot <- reactive({
      req(num_groups() > 2, group_comparison() == "parametric")
      
      # Perform one-way ANOVA
      formula_str <- paste(columnInput(), "~ cell")
      aov_formula <- as.formula(formula_str)
      aov_result <- stats::aov(aov_formula, data = stats_data())
      
      # Calculate residuals
      residual_values <- residuals(aov_result)
      fitted_values <- fitted(aov_result)
      
      # Create the residuals vs. fitted plot
      ggplot2::ggplot(data = data.frame(fitted = fitted_values, residuals = residual_values), 
             aes(x = fitted, y = residuals)) +
        geom_point(size = 3, shape = 1) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(x = "Fitted Values", y = "Residuals") +
        theme_Marnie
    })
    
    output$residualsFitPlotUI <- renderUI({
      if (group_comparison() == "parametric" && num_groups() > 2 && !is.null(input$normality_test) && length(input$normality_test) > 0 ) {
        tagList(
          h4("Residuals vs. Fitted Values"),  # Dynamic heading
          plotOutput(ns("residualsFitPlot"))  # Plot output
        )
      }
    })
    
    output$residualsFitPlot <- renderPlot({
      req(res_fit_plot())
      res_fit_plot()
    }, width = 600, height = 400)
    
    # Reactive expression for performing the Shapiro-Wilk test on raw data
    test_results_shapiro <- reactive({
        req(input$normality_test == "shapiro") # Proceed only if Shapiro-Wilk is selected
      
      # Check if the column exists in the dataset
      if (!columnInput() %in% colnames(stats_data())) {
        stop(paste("Column", columnInput(), "doesn't exist in the dataset"))
      }
        data_for_shapiro <- stats_data() # Use the reactive filtered data

        # Split the data by the 'cell' factor and perform Shapiro-Wilk test for each group
        shapiro_col <- columnInput()
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
            stats::shapiro.test(grouped_data[[group_name]])
          }, error = function(e) {
            return(list(statistic = NA, p.value = NA))
          })

          # Determine if the group passes the normality test based on the p-value
          passed_test <- test_result$p.value > 0.05
          # Determine the p-value summary
          p_value_summary <- ifelse(test_result$p.value <= 0.001, "***",
                                    ifelse(test_result$p.value <= 0.01, "**",
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

    # Reactive expression to select the appropriate Shapiro-Wilk test results
    selected_shapiro_results <- reactive({
      req(input$normality_test)  # Proceed only if normality tests are selected
        if (group_comparison() == "parametric" && num_groups() > 2) {
          residuals_sw()
        } else {
          test_results_shapiro()
        }
    })

    #  Observe to display the selected Shapiro-Wilk test results
    observe({
      results_shapiro <- selected_shapiro_results()
      
      results_shapiro <- results_shapiro %>% 
        rename("P Value" = P_value, "Passed Normality Test?" = Passed_normality_test, "P Value Summary" = P_value_summary)
      
        # Dynamically create or remove the table based on selection
        output$normalityTableUI <- renderUI({
          if("shapiro" %in% input$normality_test) {
            # Check if there's an error in the results and display it, otherwise display the table
            if (is.list(results_shapiro) && !is.null(results_shapiro$error)) {
              # Output the error message if present
              tableOutput(ns("shapiroError"))
            } else {
              tagList(
                h4("Shapiro-Wilk Normality Test"),  # Dynamic heading
                # Construct and render the results table
                dataTableOutput(ns("normalityTable"))
              )
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
      
    #residuals qqplot
    qqPlot_residuals <- reactive({
      req(group_comparison() == "parametric", num_groups() > 2)
      
      # Perform one-way ANOVA
      formula_str <- paste(columnInput(), "~ cell")
      aov_formula <- as.formula(formula_str)
      aov_result <- stats::aov(aov_formula, data = stats_data())
      
      # Calculate residuals
      residual_values <- residuals(aov_result)
      
      # Create QQ plot for residuals
      ggplot2::ggplot(data = data.frame(sample = residual_values), aes(sample = sample)) +
        geom_qq() +
        geom_qq_line() +
        labs(x = "Theoretical Quantiles", y = "Residuals") +
        theme_Marnie
    })
    
    # generate the QQ plot reactive expression
    qqPlot_reactive <- reactive({
      req("qqplot" %in% input$normality_test)  # Proceed only if QQ plot is selected
      if (group_comparison() == "parametric" && num_groups() > 2) {
        qqPlot_residuals()  # Use the residuals QQ plot
      } else {
        req("qqplot" %in% input$normality_test, !is.null(columnInput()))
        qqplot_data <- stats_data()

        ggplot2::ggplot(qqplot_data, aes(sample = !!as.symbol(columnInput()))) +
          geom_qq() + geom_qq_line() +
          facet_wrap(~cell, scales = "free_y") +
          labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
          theme_Marnie
      }
    })
    
    

    
    
    # render the QQ plot
    output$qqPlot <- renderPlot({
        req(qqPlot_reactive())
        qqPlot_reactive()
      })

    # render the QQ plot UI
    output$qqPlotUI <- renderUI({
        if (!is.null(input$normality_test) && length(input$normality_test) > 0 && "qqplot" %in% input$normality_test) {
          tagList(
            h4("QQ Plot"),  # Dynamic heading
          plotOutput(ns("qqPlot"))
          )
        }
      })
    
    densityPlot_residuals <- reactive({
      req(group_comparison() == "parametric", num_groups() > 2)
      
      # Perform one-way ANOVA
      formula_str <- paste(columnInput(), "~ cell")
      aov_formula <- as.formula(formula_str)
      aov_result <- stats::aov(aov_formula, data = stats_data())
      
      # Calculate residuals
      residual_values <- residuals(aov_result)
      
      # Create density plot for residuals
      ggplot(data = data.frame(residuals = residual_values), aes(x = residuals)) +
        geom_density() +
        labs(x = "Residuals", y = "Density") +
        theme_Marnie
    })
    
    densityPlot_reactive <- reactive({
      if (group_comparison() == "parametric" && num_groups() > 2) {
        densityPlot_residuals()  # Use the residuals density plot
      } else {
        req(input$normality_test == "density", columnInput())
        density_data <- stats_data()

        ggplot2::ggplot(density_data, aes(x = !!as.symbol(columnInput()))) +
          geom_density() +
          facet_wrap(~cell, scales = "free_y") +
          labs(x = "Value", y = "Density") +
          theme_Marnie
      }
    })


    # render the density plot
    output$densityPlot  <- renderPlot({
        req(densityPlot_reactive())
        densityPlot_reactive()
      })

    # render the density plot UI
    output$densityPlotUI <- renderUI({
        if (!is.null(input$normality_test) && length(input$normality_test) > 0 && "density" %in% input$normality_test) {
          tagList(
            h4("Density Plot"),  # Dynamic heading
            plotOutput(ns("densityPlot"))
          )
        }
      })

    observeEvent(selected_stat(), {
      # Clear all selections in 'normality_test' checkbox group
      updateCheckboxGroupInput(session, "normality_test", selected = character(0))
    })
    
    res_fit_plot_rmd <- reactive({
      if (!is.null(res_fit_plot())) {
        res_fit_plot()
      } else {
        NULL
      }
    })

    normality_input <- reactive({
      tests_selected <- input$normality_test
      
      if (group_comparison() == "parametric" && num_groups() > 2 && any(input$normality_test %in% c("qqplot", "density", "shapiro"))) {
        tests_selected <- c(tests_selected, "res_fit_plot_rmd")
      }
      
      tests_selected
    })
    
    
    normalityResults <- reactive({
      list(
        shapiro = if ("shapiro" %in% input$normality_test) selected_shapiro_results() else NULL,
        res_fit_plot_rmd = if (group_comparison() == "parametric" && num_groups() > 2) res_fit_plot_rmd() else NULL,
        qqPlot = if ("qqplot" %in% input$normality_test) qqPlot_reactive() else NULL,
        densityPlot = if ("density" %in% input$normality_test) densityPlot_reactive() else NULL
      )
    })

    return(list(
      normalityResults = normalityResults,
      normality_input = normality_input
    ))
  })
}