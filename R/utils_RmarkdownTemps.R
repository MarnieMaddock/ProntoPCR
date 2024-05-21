# utils_RmarkdownTemps.R

#download stats button

downloadStatsButton <- function(input){
  renderUI({
    # Check if group comparison has been selected
    if (!is.null(input$group_comparison) && input$group_comparison %in% c("parametric", "non_parametric", "welch")) {
      # Render the download button if a group comparison is selected
      downloadButton("downloadStats", "Download Statistics Report")
    }
  })
}


create_downloadStatsReport <- function(input, output, selected_gene_name_stats, sampleSizeTable, test_results_shapiro, 
                                       qqPlot_reactive, densityPlot_reactive, levene_reactive, comparisonResults, testName, postHocTestDescription) {
#download stats report of html Rmarkdown file
  output$downloadStats <- downloadHandler(
    filename = function() {
      #load the filename
      paste("StatisticsReport-", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".html", sep="")
    },
    content = function(file) {
      # Print dcq or ddcq
      analysisHTML <- if (input$select_dcq_or_ddcq_stats == "dcq_stats") {
        "2<sup>-(ΔCq)</sup>"
      } else if (input$select_dcq_or_ddcq_stats == "ddcq_stats") {
        "2<sup>-(ΔΔCq)</sup>"
      } else {
        ""  # Default or error case
      }
      # Get the cleaned gene name
      cleanedGeneName <- isolate(selected_gene_name_stats())
      
      # Only attempt to retrieve the sample size data if that component is selected
      # Generate the sample size table data
      sampleSizeData <- if(input$sample_size){
        sampleSizeTable()  # Ensure this is not reactive here, or use isolate() if needed
      } else {
        NULL
      }
      #generate the shapiro-wilk table data
      shapiroTable <- if("shapiro" %in% input$normality_test){
        isolate(test_results_shapiro()) 
      } else {
        NULL
      }
      #generate the qq plot
      qqPlot <- if("qqplot" %in% input$normality_test){
        isolate(qqPlot_reactive())
      } else {
        NULL
      }
      #denisty plot
      den <- if("density" %in% input$normality_test){
        isolate(densityPlot_reactive())
      } else {
        NULL
      }
      
      lev <- if(input$variance == TRUE){
        isolate(levene_reactive())
      } else {
        NULL
      }
      
      comparisonResultsData <- if(input$group_comparison == "parametric" || input$group_comparison == "non_parametric"){
        isolate(comparisonResults())
      } else {
        NULL
      }
      
      testNames <- if(input$group_comparison == "parametric" || input$group_comparison == "non_parametric"){
        isolate(testName())
      } else {
        NULL
      }
      postHocNames <- if(input$group_comparison == "parametric" || input$group_comparison == "non_parametric"){
        isolate(postHocTestDescription())
      } else {
        NULL
      }
      # Specify the path to your R Markdown template
      rmdTemplate <- "StatisticsReport.Rmd"
      
      # Render the Rmd file, passing the sample size table data as a parameter
      rmarkdown::render(input = rmdTemplate, 
                        output_file = file,
                        params = list(
                          analysisType = analysisHTML,
                          selectedSamples = input$sampleInput,
                          selectedGene = cleanedGeneName,
                          sampleSizeTable = sampleSizeData,
                          shapiroData = shapiroTable,
                          qqPlotGraph = qqPlot,
                          densityPlot = den,
                          leveneData = lev,
                          logTransformed = input$log_transform,
                          testResultData = comparisonResultsData$test,
                          cldData = comparisonResultsData$cld,
                          posthocData = comparisonResultsData$posthoc,
                          testName = testNames,
                          postHocName = postHocNames))
    }
  )
}





# Function to create the download handler for graph options
create_downloadGraphOptions <- function(input, output) {
  output$downloadGraphOptions <- downloadHandler(
    filename = function() {
      # Generate the filename
      paste("GraphOptions-", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Specify the path to your R Markdown template
      rmdTemplate <- "GraphOptions.Rmd"
      
      #Render the Rmd file, passing the sample size table data as a parameter
          rmarkdown::render(input = rmdTemplate,
                            output_file = file,
                            params = list(
                              rotateXLabels = input$rotate_labels,
                              yAxisLabel = input$y_label,
                              xAxisLabel = input$x_label,
                              font = input$font_selector,
                              xAxisTitleFontSize = input$x_axis_title_font_size,
                              yAxisTitleFontSize = input$y_axis_title_font_size,
                              xAxisTextFontSize = input$x_axis_label_font_size,
                              yAxisTextFontSize = input$y_axis_label_font_size,
                              colourScheme = input$color_scheme_select,
                              plotType = input$plot_type,
                              significance = input$add_significance,
                              yPositionSignificance = input$yPos,
                              labelSize = input$sigSize,
                              bracketThickness = input$bracketSize,
                              tipLength = input$tipLength,
                              stepIncrease = input$stepIncrease,
                              hideNS = input$hideNS,
                              pValuePrefix = input$pValuePrefix,
                              pValueDecimals = input$pValueDecimals,
                              removeLeadingZero = input$remove0,
                              errorBarType = input$error_type,
                              errorBarColour = input$match_colour_error,
                              startAtZero = input$start_at_zero,
                              errorBarWidth = input$errorbar_width,
                              errorBarThickness = input$errorbar_thickness,
                              errorBarWidthdot = input$error_bar_width,
                              errorBarThicknessdot = input$error_bar_thickness,
                              averageLineWidth = input$average_line_width,
                              averageLineThickness = input$average_line_thickness,
                              averageLineColour = input$match_colour_avg,
                              fillOrBorder = input$fill_color_toggle,
                              pointSize = input$dot_size,
                              pointSizedot = input$point_size,
                              pointSpacing = input$dot_spacing,
                              changeToPairedShapes = input$change_shapes,
                              shapeOutlineThickness = input$stroke_thickness,
                              pointSpread = input$jitter_amount,
                              seedNumber = input$seed_input,
                              fileFormat = input$file_format,
                              dpi = input$dpi,
                              width = input$width,
                              height = input$height))
    }
  )
}
