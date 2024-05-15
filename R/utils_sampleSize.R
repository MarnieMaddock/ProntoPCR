# utils_sampleSize.R

#  Function to dynamically render the heading based on the checkbox
render_sample_size_heading <- function(input){
  renderUI({
    if (input$sample_size) {
      h4(HTML("<b>Sample Size</b>"))
    }
  })
}


# Function to calculate the count of non-NA values for each sample
calculate_sample_counts <- function(input, stats_data) {
  reactive({
    req(input$sampleInput, input$columnInput)
    selected_data <- stats_data()[stats_data()$cell %in% input$sampleInput, ]
    counts <- tapply(selected_data[[input$columnInput]], selected_data$cell, function(x) sum(!is.na(x)))
    counts[names(counts) %in% input$sampleInput]
  })
}

# Function to create a table with sample sizes
create_sample_size_table <- function(sampleCounts) {
  reactive({
    counts <- sampleCounts()
    if (is.null(counts) || length(counts) == 0) {
      return(data.frame(Sample = character(0), N = integer(0)))
    }
    data.frame(Sample = names(counts), N = counts, stringsAsFactors = FALSE, row.names = NULL)
  })
}

# Function to render the table output using the sample size table
render_sample_size_table <- function(input, sampleSizeTable) {
  renderDataTable({
    if (input$sample_size) {
      datatable <- sampleSizeTable()
      if (nrow(datatable) == 0) {
        return(data.frame(Sample = "No data available", N = NA))
      }
      datatable
    } else {
      return(NULL)
    }
  }, options = list(pageLength = 5))
}

