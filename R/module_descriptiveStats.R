# module_descriptiveStats.R

descriptiveSidebar <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxGroupInput(ns("descriptiveStats"), 
                       label = HTML(
                         paste0(
                           "<b>1. Descriptive Statistics:  </b>",
                           tags$i(
                             class = "glyphicon glyphicon-info-sign", 
                             style = "color:#00359bff;", 
                             title = "Note: If log transform is selected, descriptive statistics will be performed on the log10 data."
                           )
                         )
                           ), 
                       choices = c("Sample size" = "sample_size", "Mean" = "mean", "Geometric Mean" = "geo_mean", "Median" = "median", "Standard Deviation" = "sd", "Standard Error" = "se", "95% Confidence Interval of the mean" = "ci", "Variance" = "variance", "Minimum/Maximum" = "minMax"),
                       selected = NULL)
  )
}



descriptiveMain <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("descriptivesHeading")),
    DT::DTOutput(ns("nTable")), #display sample size table if the option is checked in the sidepanel
    tags$br(),
    
  )
}


descriptiveServer <- function(id, sampleInput, columnInput, stats_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Function to dynamically render the heading based on the checkbox
    output$descriptivesHeading <- renderUI({
      if (length(input$descriptiveStats) > 0) {
        h4(HTML("<b>Descriptive Statistics</b>"))
      }
    })

    # Function to calculate statistics for each sample
    sampleStats <- reactive({
      req(sampleInput(), columnInput(), stats_data(), input$descriptiveStats)
      
      selected_data <- stats_data()[stats_data()$cell %in% sampleInput(), ]
      # Initialize an empty list to store statistics
      stats_list <- list()

      if ("sample_size" %in% input$descriptiveStats) {
        counts <- tapply(selected_data[[columnInput()]], selected_data$cell, function(x) sum(!is.na(x)))
        stats_list$N <- counts[names(counts) %in% sampleInput()]
      }

      if ("sd" %in% input$descriptiveStats) {
        sds <- tapply(selected_data[[columnInput()]], selected_data$cell, sd, na.rm = TRUE)
        stats_list$SD <- sds[names(sds) %in% sampleInput()]
      }
      
      if ("mean" %in% input$descriptiveStats) {
        means <- tapply(selected_data[[columnInput()]], selected_data$cell, mean, na.rm = TRUE)
        stats_list$Mean <- means[names(means) %in% sampleInput()]
      }
      
      if("geo_mean" %in% input$descriptiveStats) {
        geo_means <- tapply(selected_data[[columnInput()]], selected_data$cell, function(x) exp(mean(log(x), na.rm = TRUE)))
        stats_list$Geo_Mean <- geo_means[names(geo_means) %in% sampleInput()]
      }

      if ("median" %in% input$descriptiveStats) {
        medians <- tapply(selected_data[[columnInput()]], selected_data$cell, median, na.rm = TRUE)
        stats_list$Median <- medians[names(medians) %in% sampleInput()]
      }
      
      if ("se" %in% input$descriptiveStats) {
        sds <- tapply(selected_data[[columnInput()]], selected_data$cell, sd, na.rm = TRUE)
        ns <- tapply(selected_data[[columnInput()]], selected_data$cell, function(x) sum(!is.na(x)))
        ses <- sds / sqrt(ns)
        stats_list$SE <- ses[names(ses) %in% sampleInput()]
      }
      
      if ("ci" %in% input$descriptiveStats) {
        means <- tapply(selected_data[[columnInput()]], selected_data$cell, mean, na.rm = TRUE)
        sds <- tapply(selected_data[[columnInput()]], selected_data$cell, sd, na.rm = TRUE)
        valid_counts <- tapply(selected_data[[columnInput()]], selected_data$cell, function(x) sum(!is.na(x)))
        
        ci_lower <- numeric(length(means))  # Initialize with numeric vector
        ci_upper <- numeric(length(means))  # Initialize with numeric vector
        
        # Iterate over sample names instead of indices
        sample_names <- names(means)
        for (sample_name in sample_names) {
          n <- valid_counts[sample_name]
          if (!is.na(n) && n > 0) {
            mean <- means[sample_name]
            sd <- sds[sample_name]
            se <- sd / sqrt(n)
            if (n >= 30) {
              z_value <- 1.96
              ci_lower[sample_name] <- mean - z_value * se
              ci_upper[sample_name] <- mean + z_value * se
            } else {
              t_value <- qt(0.975, df = n - 1, lower.tail = TRUE)
              ci_lower[sample_name] <- mean - t_value * se
              ci_upper[sample_name] <- mean + t_value * se
            }
          } else {
            ci_lower[sample_name] <- NA
            ci_upper[sample_name] <- NA
          }
        }
        
        stats_list$CI_Lower <- ci_lower[names(ci_lower) %in% sampleInput()]
        stats_list$CI_Upper <- ci_upper[names(ci_upper) %in% sampleInput()]
      }
      
      if ("variance" %in% input$descriptiveStats) {
        variances <- tapply(selected_data[[columnInput()]], selected_data$cell, var, na.rm = TRUE)
        stats_list$Variance <- variances[names(variances) %in% sampleInput()]
      }
      
      if ("minMax" %in% input$descriptiveStats) {
        mins <- tapply(selected_data[[columnInput()]], selected_data$cell, min, na.rm = TRUE)
        maxs <- tapply(selected_data[[columnInput()]], selected_data$cell, max, na.rm = TRUE)
        stats_list$Min <- mins[names(mins) %in% sampleInput()]
        stats_list$Max <- maxs[names(maxs) %in% sampleInput()]
      }

      return(stats_list)
    })


    # Function to create a table with sample sizes and other statistics
    descriptivesTable <- reactive({
      stats <- sampleStats()
      
      # Ensure there's data to display
      #sample_names <- unique(stats_data()$cell)
      sample_names <- sampleInput()  # Use sampleInput to get selected sample names
      if (is.null(sample_names) || length(sample_names) == 0) {
        return(data.frame(Sample = "No data available", stringsAsFactors = FALSE))
      }
      
      # Create a base data frame with Sample names
      result_df <- data.frame(Sample = sample_names, stringsAsFactors = FALSE)
      
      # Adding selected statistics
      if ("sample_size" %in% input$descriptiveStats) {
       # result_df$N <- stats$N[names(stats$N) %in% sample_names]
        result_df$N <- stats$N[sample_names]
      } 
      
      if ("mean" %in% input$descriptiveStats) {
        result_df$Mean <- stats$Mean[sample_names]
      }
      
      if ("geo_mean" %in% input$descriptiveStats) {
        result_df$"Geometric Mean" <- stats$Geo_Mean[sample_names]
      }
      
      if ("median" %in% input$descriptiveStats) {
        result_df$Median <- stats$Median[sample_names]
      }
      
      if ("sd" %in% input$descriptiveStats) {
        result_df$SD <- stats$SD[sample_names]
      }
      
      if ("se" %in% input$descriptiveStats) {
        result_df$SE <- stats$SE[sample_names]
      }
      
      if ("ci" %in% input$descriptiveStats) {
        result_df$`CI Lower` <- stats$CI_Lower[sample_names]
        result_df$`CI Upper` <- stats$CI_Upper[sample_names]
      }
      
      if ("variance" %in% input$descriptiveStats) {
        result_df$Variance <- stats$Variance[sample_names]
      }
      
      if ("minMax" %in% input$descriptiveStats) {
        result_df$Min <- stats$Min[sample_names]
        result_df$Max <- stats$Max[sample_names]
      }
      
      return(result_df)
    })

    # Function to render the table output using the sample size table
    output$nTable <- DT::renderDT({
      req(input$descriptiveStats)
      datatable <- descriptivesTable()
      if (nrow(datatable) == 0) {
        return(data.frame(Sample = "No data available", N = NA))
      }
      datatable
    }, options = list(pageLength = 5))

    # Compute sample sizes independently
    computeSampleSizes <- reactive({
      selected_data <- stats_data()
      
      # Compute counts based on non-NA values in each group
      counts <- tapply(selected_data[[columnInput()]], selected_data$cell, function(x) sum(!is.na(x)))
      
      # Filter counts by sampleInput()
      sample_names <- unique(selected_data$cell)
      sizes <- counts[names(counts) %in% sample_names]
      
      return(sizes)
    })
    
    descriptivesResults <- reactive({
      if (length(input$descriptiveStats) > 0) {
        descriptivesTable()  # Assuming descriptivesTable() returns the descriptive statistics table
      } else {
        NULL
      }
    })
    
    descriptives_input <- reactive({
      input$descriptiveStats
    })
    
    
    return(list(descriptivesTable = descriptivesTable,
                descriptivesResults = descriptivesResults,
                descriptives_input = descriptives_input,
                computeSampleSizes = computeSampleSizes))
  })
}
