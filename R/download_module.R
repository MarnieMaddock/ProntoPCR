# download_module.R

downloadModule <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Add a download handler for the graph
    output$downloadGraph <- downloadHandler(
      filename = function() {
        paste("your_graph_filename", ".", input$file_format, sep = "")
      },
      content = function(file) {
        # Use the `ggsave` function to save the plot as an SVG file
        ggsave(file, plot = last_plot(), device = input$file_format, dpi = input$dpi, width = input$width, height = input$height)
      }
    )
    
    # Add download handler for processed data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("processed_PCR_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(wrangled_data(), file, row.names = FALSE)
      }
    )
    
    # Add download handler for filtered data
    output$downloadFilteredData <- downloadHandler(
      filename = function() {
        condition <- input$condition
        if (!is.null(condition)) {
          paste("processed_PCR_data_", condition, "_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("processed_PCR_data_", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE)
      }
    )
    
    # Add download handler for replicate average data
    output$rep_avg_download <- downloadHandler(
      filename = function() {
        paste("Replicate_avg_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(rep_avg_data(), file, row.names = FALSE)
      }
    )
    
    # Add download handler for filtered replicate average data
    output$rep_avg_filtered_download <- downloadHandler(
      filename = function() {
        condition <- input$condition
        if (!is.null(condition)) {
          paste("Replicate_avg_data_", condition, "_filtered_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("Replicate_avg_data_filtered_", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        write.csv(filtered_rep_avg_data(), file, row.names = FALSE)
      }
    )
    
  })
}
