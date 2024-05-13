# module_checkCSVfile.R
checkCSVfileUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Choose CSV File", accept = c(".csv")), #input csv files
    helpText("Please select a CSV file containing PCR data with the formatting given in the Example Data Tab."),
    helpText("If you have 'undetermined' amplification i.e. no amplification, please enter 0 in your dataset in the Cq.Mean column. Any NA values will be disregarded (i.e. REMOVED)."),
    tags$br()
  )
}

# function that reads a csv file and checks if it meets certain requirements to run the app.
checkCSVfile <- function(id, data) {
  moduleServer(id = "file", function(input, output, session) {
    # Reactive function to read and display the uploaded CSV file
    data <- reactive({
      req(input$file)
      
      # Read the CSV file
      df <- read.csv(input$file$datapath, na.strings = c("", "NA"))
      
      # Expected column names
      expected_colnames <- c("Sample", "Target", "Num.of.Replicates", "Cq.Mean", "Cq.SD", "Result.Quality.Issues")
      
      # Check if all expected columns exist in the uploaded data
      if (!all(expected_colnames %in% names(df))) {
        missing_colnames <- expected_colnames[!expected_colnames %in% names(df)]
        # Show an error modal if there are missing columns
        showModal(modalDialog(
          title = "Error in uploaded file",
          HTML(paste("The uploaded file is missing the following columns:", paste(missing_colnames, collapse=", ")),
               "<br><br>Please include all necessary columns and try again. This error may persist if spaces or incorrect spelling/capitalisation is also present. <br><br>The expected columns are: Sample, Target, Num.of.Replicates, Cq.Mean, Cq.SD, Result.Quality.Issues."),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return(NULL)  # Return NULL to stop further processing if columns are missing
      }
      
      # Check for any empty rows (all values are NA or blank in that row)
      empty_rows <- which(rowSums(is.na(df)) == ncol(df))
      if (length(empty_rows) > 0) {
        # Adjusting the row numbers for the error message
        corrected_rows <- empty_rows + 1
        showModal(modalDialog(
          title = "Empty Rows Detected",
          HTML(paste("The uploaded file contains empty rows at positions:", paste(corrected_rows, collapse=", "), "<br><br>Please remove or fill these rows and try again.")),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return(NULL)  # Return NULL to stop further processing if empty rows are detected
      }
      
      # Check for duplicate rows
      dup_rows <- which(duplicated(df) | duplicated(df, fromLast = TRUE))
      if (length(dup_rows) > 0) {
        corrected_dup_rows <- dup_rows + 1  # Adjust for header row
        showModal(modalDialog(
          title = "Duplicate Rows Detected",
          HTML(paste("The uploaded file contains duplicate rows at positions:", paste(corrected_dup_rows, collapse=", "), "<br><br>Please remove or correct these duplicates and try again.")),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return(NULL)  # Return NULL to stop further processing if duplicate rows are detected
      }
      
      # Check for duplicate rows based on non-numeric columns i.e. if the Target and sample is the same, but they have different Cq values
      if (any(duplicated(df[, c("Sample", "Target")]))) {
        dup_rows <- which(duplicated(df[, c("Sample", "Target")]) | duplicated(df[, c("Sample", "Target")], fromLast = TRUE))
        corrected_dup_rows <- dup_rows + 1
        showModal(modalDialog(
          title = "Duplicate Rows Detected",
          HTML(paste("The uploaded file contains rows with identical Sample and Target names, but different Cq values at positions:", paste(corrected_dup_rows, collapse=", "), "<br><br>Please check these entries for potential data errors or necessary corrections and try again. <br><br>Please ensure the data inserted is the average of the technical replicates.")),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return(NULL)
      }
      
      # Regular expression to check the format of 'Sample' column
      incorrect_format_rows <- which(!grepl("^[a-zA-Z0-9]+_\\d+_\\w+$", df$Sample))
      if (length(incorrect_format_rows) > 0) {
        corrected_format_rows <- incorrect_format_rows + 1
        incorrect_samples <- df$Sample[incorrect_format_rows]  # Extracting incorrect sample names
        # Combining row numbers with sample names for the modal dialog
        error_details <- paste(corrected_format_rows, incorrect_samples, sep=": ", collapse=", ")
        showModal(modalDialog(
          title = "Incorrect Format in Sample Column",
          HTML(paste("The following rows and their respective samples in the 'Sample' column do not match the required format 'name_number_group':", 
                     "<br>", error_details, 
                     "<br><br>Please correct these entries to match the format and try again.")),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return(NULL)  # Return NULL to stop further processing if format is incorrect
      }
      
      return(df)
    })
    
  })
}