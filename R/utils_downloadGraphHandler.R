# utils_downloadGraphHandler.R

downloadGraphHandler <- function(file_format, dpi, width, height) {
  downloadHandler(
    filename = function() {
      paste("your_graph_filename", ".", file_format, sep = "")
    },
    content = function(file) {
      # Use a switch statement to select the appropriate device for saving
      dev <- switch(file_format,
                    "svg" = "svg",
                    "png" = "png",
                    "jpeg" = "jpeg",
                    "tiff" = "tiff"
      )
      # Use the selected device to save the plot
      ggsave(file, plot = last_plot(), device = dev, dpi = dpi, width = width, height = height)
    }
  )
}



