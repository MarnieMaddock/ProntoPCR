# utils_adataWrangling.R
# caluclations for initial data wrangling and DCQ calculations
# Function to Display the inserted csv as a table using DataTable
render_data_table <- function(data){
  renderDataTable({
    data()
  }, options = list(pageLength = 5))
}


# Function to generate dynamic text input fields for housekeeper genes
generate_groups_ui <- function(input){
  renderUI({
    housekeepers <- as.integer(input$housekeepers)
    lapply(
      1:housekeepers,
      function(i){
        textInput(
          paste0("group", i),
          paste0("Enter the name of housekeeper ", i)
        )
      }
    )
  })
}



# Save the text inputs as variables when the button is clicked
save_housekeeper_names <- function(input, saved_variables) {
  observeEvent(input$save_btn,{
    housekeepers <- as.integer(input$housekeepers)
    saved_variables$names <- sapply(1:housekeepers, function(i) input[[paste0("group", i)]])
  })
}

# Function to render text output based on saved variables
render_housekeeper_text <- function(input, saved_variables) {
# Generate the output text based on the saved variables
renderText({
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
}

# Function to render instruction text
render_instruction_text <- function() {
  renderText({
    paste("Once you have saved the housekeeper gene names, please move to the calculations tab.")
  })
}

# New reactive expression for data wrangling
wrangle_data <- function(input, data, saved_variables){
  reactive({
    req(input$save_btn)
    if (is.null(data())) {
      return(NULL)
    }
    # Remove Cq.SD and Quality issues columns
    df <- data()[, c(1, 2, 4)]
    
    # Check if 'NTC' is present in the 'Sample' column and remove if present
    df <- df[!grepl('NTC', df$Sample), ]
    # Check if 'NTC' is present in the 'Target' column and remove if present
    df <- df[!grepl('NTC', df$Target), ]
    
    #make the dataframe longer (with targets as columns)
    df <- df[,1:3] %>% pivot_wider(names_from = Target, values_from = Cq.Mean)
    
    # Retrieve the saved variables
    variables <- saved_variables$names
    # Check if all provided housekeeper names exist as columns
    missing_columns <- setdiff(variables, names(df))
    if (length(missing_columns) > 0) {
      # Show an error modal if there are missing columns
      shiny::showModal(modalDialog(
        title = "Error",
        paste("The following housekeeper names are not found:", paste(missing_columns, collapse = ", ")),
        ". Please check the spelling or capitalisations of these genes in your inserted data sheet and try again.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return(NULL)  # Return NULL to prevent further processing
    }
    
    #Average the housekeepers
    df <- df %>%
      rowwise() %>%
      mutate(
        mean_hk = mean(c_across(all_of(variables)), na.rm = TRUE),
      )
    #move columns using datawizard package
    df <- data_relocate(df, select = "mean_hk", after = "Sample")
    df <- data_relocate(df, select = saved_variables$names, after = "Sample")
    
    #Calculate delta Cq for each target
    #which(names(data) == "mean_hk") finds the column index of "mean_hk" in the dataframe.
    #+ 1 increments the index to select the columns directly after "mean_hk".
    #ncol(data) provides the last column index of the dataframe.
    df <- df %>%
      mutate(across((which(names(df) == "mean_hk") + 1):ncol(df),
                    list(dcq = ~ ifelse(.x != 0, .x - mean_hk, 0)),
                    .names = "{.fn}_{.col}"))
    
    #calulcate fold change (relative mRNA)
    # Calculate fc, considering the case where the data point is 0
    #supressed warnings as it is inconsequential to the data functionality
    df <- suppressWarnings({df %>%
        mutate(across(
          (which(startsWith(names(df), "dcq_"))):ncol(df),
          list(fc = ~ ifelse(.x != 0, 2^(-.x), 0)),
          .names = "{.fn}_{.col}"
        ))
    })
    
    # Make a new column that places each sample as the specified condition
    #regex extracts characters after the last underscore
    df$condition <- gsub(".*_(\\w+)$", "\\1", df$Sample)
    df$condition <- as.factor(df$condition)
    #Add group data
    #regex extracts characters before the first underscore
    df$group <- gsub("^([^_]+)_.*$", "\\1", df$Sample)
    df$group <- as.factor(df$group)
    
    #add combined
    df$cell <- paste(df$condition, df$group, sep = "_")
    df$cell <- as.factor(df$cell)
    #Move column using datawizard package
    df <- data_relocate(df, select = "group", after = "Sample")
    df <- data_relocate(df, select = "condition", after = "Sample")
    df <- data_relocate(df, select = "cell", after = "group")
    
    return(df)
  })
}


# Function to render the calculations table
render_calculations_table <- function(wrangled_data) {
# Display the table using DataTable in "Calculations" tab
 renderDataTable({
    req(wrangled_data())
    wrangled_data()
  }, options = list(pageLength = 5, scrollX = TRUE, scrollY = "200px"))
}

#filter data wrangled by selected condition (UI component)
filter_by_conditionUI <- function(wrangled_data, input){
  renderUI({
    req(wrangled_data())
    conditions <- unique(wrangled_data()$condition)
    selectInput("condition", "Select Condition", choices = conditions)
  })
}


#Create filtered dataset
filtered_dataset <- function(wrangled_data, input){
  reactive({
    req(wrangled_data())
    conditions_to_filter <- input$condition
    
    if (!is.null(conditions_to_filter)) {
      filtered_data <- wrangled_data() %>%
        filter(condition %in% conditions_to_filter)
      return(filtered_data)
    } else {
      return(NULL)
    }
  })
}

# Display the filtered table
filtered_table_displayUI <- function(filtered_data){
  renderDataTable({
      req(filtered_data())
      filtered_data()
    }, options = list(pageLength = 5, scrollX = TRUE, scrollY = "200px"))
}

#save the condition to use in string for saving csv file
save_condition_for_download <- function(input){
  reactive({
    input$condition
  })
}


#replicate averages datasets + calcs
# Calculate replicate averages when data is loaded
perform_rep_average <- function(wrangled_data){
  reactive({
    req(wrangled_data())
    
    vars <- colnames(wrangled_data()) %>%
      grep("^fc_dcq", ., value = TRUE)
    
    # Now, the 'vars' object contains the desired column names
    rep_avg <- wrangled_data() %>%
      group_by(condition, group) %>%
      summarise_at(vars, list(fc_avg = ~mean(., na.rm = TRUE))) %>%
      gather(key = "Variable", value = "fc_avg", -condition, -group)
    
    rep_avg <- rep_avg %>% 
      pivot_wider(names_from = Variable, values_from = fc_avg)
    
    # Remove "_fc_avg" from column names
    colnames(rep_avg) <- sub("_fc_avg$", "", colnames(rep_avg))
    
    #add column cell
    rep_avg$cell <- paste(rep_avg$condition, rep_avg$group, sep = "_")
    rep_avg$cell <- as.factor(rep_avg$cell)
    #Move column
    rep_avg <- data_relocate(rep_avg, select = "cell", after = "group")
    
    return(rep_avg)
  })
}

# Display the replicate averages table in "Calculations" tab
biorep_displayUI <- function(rep_avg_data){
  renderDataTable({
    rep_avg_data()
  }, options = list(pageLength = 5, scrollX = TRUE, scrollY = "200px"))
}

# Create filtered replicate data table
filtered_rep_avg_dataset <- function(rep_avg_data, input){
  reactive({
    req(rep_avg_data())
    conditions_to_filter <- input$condition
    
    if (!is.null(conditions_to_filter)) {
      filtered_data <- rep_avg_data() %>%
        filter(condition %in% conditions_to_filter)
      return(filtered_data)
    } else {
      return(NULL)
    }
  })
}

# display rep data filtered ouput in the UI
filtered_rep_displayUI <- function(filtered_rep_avg_data){
  renderDataTable({
    req(filtered_rep_avg_data())
    filtered_rep_avg_data()
  }, options = list(pageLength = 5, scrollX = TRUE, scrollY = "200px"))
}



