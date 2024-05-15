# utils_normPlots.R

# Function to generate the QQ plot reactive expression
generate_qqPlot_reactive <- function(input, shapiro_data_reactive) {
  reactive({
    req("qqplot" %in% input$normality_test, !is.null(input$columnInput))
    qqplot_data <- shapiro_data_reactive()
    
    ggplot(qqplot_data, aes(sample = !!as.symbol(input$columnInput))) +
      geom_qq() + geom_qq_line() +
      facet_wrap(~cell, scales = "free_y") +
      labs(title = "QQ Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_Marnie
  })
}

# Function to render the QQ plot
render_qqPlot <- function(qqPlot_reactive) {
  renderPlot({
    req(qqPlot_reactive())
    qqPlot_reactive()
  })
}


# Function to generate the density plot reactive expression
generate_densityPlot_reactive <- function(input, shapiro_data_reactive) {
  reactive({
    req(input$normality_test == "density", input$columnInput)
    density_data <- shapiro_data_reactive()
    
    density_data %>% 
      ggplot(aes(x = !!as.symbol(input$columnInput))) +
      geom_density() +
      facet_wrap(~cell, scales = "free_y") +
      labs(title = "Density Plot", x = "Value", y = "Density") +
      theme_Marnie
  })
}

# Function to render the density plot
render_densityPlot <- function(densityPlot_reactive) {
  renderPlot({
    req(densityPlot_reactive())
    densityPlot_reactive()
  })
}

# Function to render the QQ plot UI
render_qqPlotUI <- function(input) {
  renderUI({
    if (!is.null(input$normality_test) && length(input$normality_test) > 0 && "qqplot" %in% input$normality_test) {
      plotOutput("qqPlot")
    }
  })
}

# Function to render the density plot UI
render_densityPlotUI <- function(input) {
  renderUI({
    if (!is.null(input$normality_test) && length(input$normality_test) > 0 && "density" %in% input$normality_test) {
      plotOutput("densityPlot")
    }
  })
}
