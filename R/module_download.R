# # module_download.R

#functions that allow downloading of csv files
downloadUI <- function(id, label = "Download Data") {
  ns <- NS(id)
  tagList(
    downloadButton(ns("download"), label)
  )
}


# Unified server function for download handlers
downloadServer <- function(id, df, filenameFunc) {
  moduleServer(id, function(input, output, session) {
    output$download <- downloadHandler(
      filename = function() {
        filenameFunc(input, session)  # Generate filename dynamically
      },
      content = function(file) {
        utils::write.csv(df(), file, row.names = FALSE)  # Assuming df is a reactive expression
      }
    )
  })
}
