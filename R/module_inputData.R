# module_inputData.R

inputFileUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label = tags$span("Choose CSV File", 
                                            tags$i(
                                              class =  "glyphicon glyphicon-info-sign", 
                                              style = "color:#00359bff;",
                                              title = "Please select a CSV file containing PCR data with the formatting given in the Example Data.")),
              accept = c(".csv")), #input csv files
    helpText("If you have 'undetermined' amplification i.e. no amplification, please enter 0 in your dataset in the Cq.Mean column. Any NA values will be disregarded (i.e. REMOVED)."),
    tags$br(),
    downloadLink(ns("download_example"), "Download Example Data"),
    tags$br(),
    tags$br(),
    numericInput(
      ns("housekeepers"),
      "How many housekeeper genes do you have?",
      3,
      min = 1,
      max = 10),
    uiOutput(ns("groups")), # Generate dynamic text input fields based on the number of groups for housekeeper genes
    helpText("Ensure that genes are entered exactly as they appear in the Target column."),
    actionButton(ns("save_btn"), "Save housekeeper names"),
    tags$br(),
    tags$br(),
    tags$br(),
    textOutput(ns("text1")), verbatimTextOutput("saved_variables"), # Generate the output text based on the saved variables
    tags$br(),

    textOutput(ns("text2"))
  )
}


downloadExampleData <- function(id, dataset_path = "inst/www/exampledata.csv") {
  moduleServer(
    id,
    function(input, output, session) {
      output$download_example  <- downloadHandler(
        filename = function() {
          "exampledata.csv"
        },
        content = function(file) {
          file.copy(dataset_path, file)
          # If the dataset is generated within the app, use something like this:
          # write.csv(dataset, file, row.names = FALSE)
        }
      )
    }
  )
}

inputDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3(HTML("Inserted Data")),
    DT::DTOutput(ns("table")),
    tags$br(),
    tags$br()
  )
}

inputFileServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    saved_variables <- reactiveValues(names = NULL)
    

    
    #display the number of housekeeper genes names depending on how many groups are selected. 
    output$groups <- renderUI({
      housekeepers <- as.integer(input$housekeepers)
      lapply(
        1:housekeepers,
        function(i){
          textInput(
            ns(paste0("group", i)),
            paste0("Enter the name of housekeeper ", i)
          )
        }
      )
    })
    
    # Save text inputs as variables when the button is clicked
    #saved_variables <- reactiveValues()
    observeEvent(input$save_btn, {
      saved_variables$names <- sapply(1:input$housekeepers, function(i) input[[ns(paste0("group", i))]])
    })
    # Save the text inputs as variables when the button is clicked
    save_housekeeper_names <- observeEvent(input$save_btn,{
        housekeepers <- as.integer(input$housekeepers)
        saved_variables$names <- sapply(1:housekeepers, function(i) input[[paste0("group", i)]])
      })
    # # Render text output based on saved variables
    output$text1 <-  renderText({
      if (is.null(input$housekeepers)) {
        return()
      }
      housekeepers <- as.integer(input$housekeepers)
      variables <- saved_variables$names
      paste0(
        "You have ", housekeepers, " housekeeper genes. ",
        "The housekeepers are called ", paste(variables, collapse = ", "), "."
      )
    })
    
    # Render instruction text
    output$text2 <- renderText({
      paste("Once you have saved the housekeeper gene names, please move to the calculations tab.")
    })
    return(list(
      saved_variables = saved_variables,
      save_btn = reactive(input$save_btn)
      #geo_mean_state = geo_mean_state
    ))
  })
}

# function that reads a csv file and checks if it meets certain requirements to run the app.
checkCSVfile <- function(id) {
  moduleServer(id, function(input, output, session) {
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
    return(list(
      data = data,
      file = reactive(input$file)
    ))
  })

}

inputDataServer <- function(id, csv_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render the data table for the uploaded CSV
    output$table <- DT::renderDT({
      req(csv_data$data())
      csv_data$data()
    }, options = list(pageLength = 5))
  })
}
