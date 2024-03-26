# Load the required libraries
source("module_download.R")
source("utils_downloadGraphHandler.R")

#Stats functions
source("utils_performComparisonTests.R")
source("utils_performCLD.R")
source("utils_performTukeyPostHoc.R")
source("utils_performPostHoc.R")
source("utils_performDunnPostHoc.R")
source("utils_performConoverPostHoc.R")

#Graph functions
source("utils_graphTheme.R")
source("utils_getColourSchemes.R")

server <- function(input, output, session) {
  

  # Reactive function to read and display the uploaded CSV file
  data <- reactive({
    req(input$file)
    
    # Read the CSV file
    df <- read.csv(input$file$datapath)
    
    return(df)
  })
  
  # Display the inserted csv as a table using DataTable
  output$table <- renderDataTable({
    data()
  }, options = list(pageLength = 5))
  
  # Reactive values to store housekeeper names and numeric input value
  housekeepers_names <- reactiveValues()
  
  # Generate dynamic text input fields based on the number of groups for housekeeper genes
  output$groups <- renderUI({
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
  
  # Save the text inputs as variables when the button is clicked
  saved_variables <- reactiveValues()
  observeEvent(input$save_btn, {
    housekeepers <- as.integer(input$housekeepers)
    saved_variables$names <- sapply(1:housekeepers, function(i) input[[paste0("group", i)]])
  })
  
  # Generate the output text based on the saved variables
  output$text1 <- renderText({
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
  
  
  output$text2 <- renderText({
    paste("Once you have saved the housekeeper gene names, please move to the calculations tab.")
  })
  
  
  # New reactive expression for data wrangling
  wrangled_data <- reactive({
    req(input$save_btn)
    if (is.null(data())) {
      return(NULL)
    }
    
    # Remove Cq.SD and Quality issues columns
    df <- data()[, c(1, 2, 4)]
    
    # Check if 'NTC' is present in the 'Sample' column and remove if present
    df <- df[!grepl('NTC', df$Target), ]
    
    #make the dataframe longer (with targets as columns)
    df <- df[,1:3] %>% pivot_wider(names_from = Target, values_from = Cq.Mean)
    
    # Retrieve the saved variables
    variables <- saved_variables$names
    
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
  
  #download processed data as a csv.
  downloadServer("download_processed_data", wrangled_data, function(input, session) {
    paste("processed_PCR_data_", Sys.Date(), ".csv", sep = "")
  })
  
  # Display the table using DataTable in "Calculations" tab
  output$calculations_table <- renderDataTable({
    req(wrangled_data())
    wrangled_data()
  }, options = list(pageLength = 5, scrollX = TRUE, scrollY = "200px"))
  
  # filter data by condition
  output$condition_filter <- renderUI({
    req(wrangled_data())  # Ensure data is available
    conditions <- unique(wrangled_data()$condition)
    
    selectInput("condition", "Select Condition", choices = conditions)
  })
  
  filtered_data <- reactive({
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
  # Display the filtered table
  output$filtered_table <- renderDataTable({
    req(filtered_data())
    filtered_data()
  }, options = list(pageLength = 5, scrollX = TRUE, scrollY = "200px"))
  
  #save the condition to use in string for saving csv file
  condition_for_download <- reactive({
    input$condition
  })
  
  downloadServer("download_filtered_data", filtered_data, function(input, session) {
    if (!is.null(condition_for_download())) {
      paste("filtered_PCR_data_", condition_for_download(), "_", Sys.Date(), ".csv", sep = "")
    } else {
      paste("filtered_PCR_data_", Sys.Date(), ".csv", sep = "")
    }
  })
  
  # Calculate replicate averages when data is loaded
  rep_avg_data <- reactive({
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
  
  
  # Display the replicate averages table in "Calculations" tab
  output$rep_avg_table <- renderDataTable({
    rep_avg_data()
  }, options = list(pageLength = 5, scrollX = TRUE, scrollY = "200px"))
  
  downloadServer("download_rep_avg_data", rep_avg_data, function(input, session) {
    paste("Replicate_avg_data_", Sys.Date(), ".csv", sep = "")
  })
  
  
  
  output$rep_avg_filtered_table <- renderDataTable({
    req(filtered_rep_avg_data())
    filtered_rep_avg_data()
  }, options = list(pageLength = 5, scrollX = TRUE, scrollY = "200px"))
  
  filtered_rep_avg_data <- reactive({
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
  
  downloadServer("download_rep_avg_filtered_data", filtered_rep_avg_data, function(input, session) {
    if (!is.null(condition_for_download())) {
      paste("Replicate_avg_data_", condition_for_download(), "_", Sys.Date(), ".csv", sep = "")
    } else {
      paste("Replicate_avg_data_filtered_", Sys.Date(), ".csv", sep = "")
    }
  })
  
  
  # DELTADELTA 
  #
  #
  output$select_condition <- renderUI({
    req(wrangled_data())
    selectInput("select_condition", "Select Condition", choices = unique(wrangled_data()$condition))
  })
  
  output$select_control <- renderUI({
    req(wrangled_data())  # Ensure data is available
    
    selectInput("select_control", "Select the control/untreated sample", choices = unique(wrangled_data()$group))
  })
  
  output$select_samples <- renderUI({
    req(wrangled_data())  # Ensure data is available
    
    selectInput("select_samples", "Select the diseased/treated sample(s)", choices = unique(wrangled_data()$group), multiple = T)
  })
  
  output$column_selector2 <- renderUI({
    req(wrangled_data())
    
    # Filter column names to include only those starting with "dcq"
    dcq_columns <- grep("^dcq_", colnames(wrangled_data()), value = TRUE)
    
    # Generate selectInput for choosing the column dynamically
    selectInput("select_gene", "Select Gene to calculate ΔΔCq", choices = dcq_columns)
  })
  
  ddcq_filtered_data <- reactive({
    req(wrangled_data())
    req(input$select_gene)
    
    condition2 <- input$select_condition
    control <- input$select_control
    samples <- input$select_samples
    selected_gene <- input$select_gene
    
    ddcq_data <- wrangled_data() %>% 
      filter((group == control) | (group %in% samples)) %>% 
      filter(condition == condition2) %>%
      dplyr::select(group, condition, all_of(selected_gene))
    
    # Resetting levels of factors to only include selected options
    ddcq_data$group <- factor(ddcq_data$group, levels = unique(c(as.character(control), as.character(samples))))
    ddcq_data$condition <- factor(ddcq_data$condition, levels = condition2)
    return(ddcq_data)
  })
  
  mean_value <- reactiveVal(NULL)
  # Calculate the average delta cq for the selected gene in the control samples
  average_dcq <- reactive({
    req(wrangled_data())
    req(input$select_gene)
    req(input$select_control)
    
    condition3 <- input$select_condition
    selected_gene2 <- input$select_gene
    control2 <- input$select_control
    samples2 <- input$select_samples
    
    # Calculate the average delta cq for the selected gene in the control samples
    avg_dcq_ctrl <- ddcq_filtered_data() %>%
      filter(group == control2) %>%
      group_by(group, condition) %>%
      summarise(dcq_ctrl_avg = mean(!!sym(selected_gene2), na.rm = TRUE), .groups = "drop")
    
    # Left join the original dataframe with the summarised dataframe
    avg_dcq_ctrl <- left_join(ddcq_filtered_data(), avg_dcq_ctrl, by = c("group", "condition"))
    
    # Calculate the mean value
    mean_val <- mean(avg_dcq_ctrl$dcq_ctrl_avg, na.rm = TRUE)
    
    # Store the mean value in the reactive value
    mean_value(mean_val)
    
    # Assign the mean value to the entire dcq_ctrl_avg column
    avg_dcq_ctrl$dcq_ctrl_avg <- mean_val
    
    # Create a new column ddcq by subtracting selected_gene2 from dcq_ctrl_avg
    avg_dcq_ctrl$ddcq <-  avg_dcq_ctrl[[selected_gene2]] - avg_dcq_ctrl$dcq_ctrl_avg
    
    # Create a new column fc_ddcq containing 2^(-ddcq)
    avg_dcq_ctrl$fc_ddcq <- 2^(-avg_dcq_ctrl$ddcq)
    
    # Resetting levels of factors to only include selected options
    avg_dcq_ctrl$group <- factor(avg_dcq_ctrl$group, levels = unique(c(as.character(control2), as.character(samples2))))
    avg_dcq_ctrl$condition <- factor(avg_dcq_ctrl$condition, levels = condition3)
    avg_dcq_ctrl$cell <- paste(avg_dcq_ctrl$condition, avg_dcq_ctrl$group, sep = "_")
    return(avg_dcq_ctrl)
  })
  
  output$ddcq_data <- renderDataTable({
    req(average_dcq())
    average_dcq()
  }, options = list(pageLength = 5))
  
  downloadServer("download_ddcq_data", ddcq_data, function(input, session) {
    paste("DDCQ_processed_data_", Sys.Date(), ".csv", sep = "")
  })
  
  values <- reactiveValues(ddcqDataSaved = FALSE)
  
  observeEvent(input$save_ddcq_data, {
    # This block is executed whenever the 'Save ddcq Data' button is clicked.
    # Even though you don't want to perform any action immediately when the button is clicked,
    # use this as a trigger for other reactive expressions or observers.
    values$ddcqDataSaved <- TRUE
  })
  
  # Calculate replicate averages when data is loaded
  
  rep_avg_data_ddcq <- reactive({
    req(average_dcq())
    
    rep_avg_ddcq <- average_dcq() %>%
      group_by(condition, group) %>%
      summarize(mean_fc_ddcq = mean(fc_ddcq, na.rm = TRUE), .groups = "drop")
    
    #add column cell
    rep_avg_ddcq$cell <- paste(rep_avg_ddcq$condition, rep_avg_ddcq$group, sep = "_")
    rep_avg_ddcq$cell <- as.factor(rep_avg_ddcq$cell)
    #Move column
    rep_avg_ddcq <- data_relocate(rep_avg_ddcq, select = "cell", after = "group")
    return(rep_avg_ddcq)
  })
  
  # Display the replicate averages table in "Calculations" tab
  output$rep_avg_table_ddcq <- renderDataTable({
    rep_avg_data_ddcq()
  }, options = list(pageLength = 5))
  
  downloadServer("download_ddcq_avg_data", rep_avg_table_ddcq, function(input, session) {
    paste("DDCq_processed_replicate_data_", Sys.Date(), ".csv", sep = "")
  })
  
  #Stats
  observeEvent(input$select_dcq_or_ddcq_stats, {
    # Check if 'ddcq' is selected
    if (input$select_dcq_or_ddcq_stats == "ddcq_stats") {
      # Display the gene selection message
      output$selected_gene_ui_stats <- renderUI({
        textOutput("selected_gene_message_stats")
      })
    } else {
      # Hide the message when 'dcq' is selected or for any other condition
      output$selected_gene_ui_stats <- renderUI({})
    }
  })
  
  # Define the text output for displaying the selected gene
  output$selected_gene_message_stats <- renderText({
    req(input$select_gene)  # Ensure there is a selection
    # Use gsub to remove "dcq_" from the selected gene's name
    selected_gene_cleaned <- gsub("^dcq_", "", input$select_gene)
    paste("You are currently performing stats on gene:", selected_gene_cleaned)
  })
  
  observe({
      if (input$select_dcq_or_ddcq_stats == "dcq_stats") {
        updateSelectInput(session, "sampleInput", choices = unique(wrangled_data()$cell))
        updateSelectInput(session, "columnInput", choices = grep("^fc_dcq_", names(wrangled_data()), value = TRUE))
      } else if (input$select_dcq_or_ddcq_stats == "ddcq_stats") {
        # Check if avg_dcq_df is not NULL and has the expected columns
        if (!is.null(values$ddcqDataSaved) && values$ddcqDataSaved) {
        updateSelectInput(session, "sampleInput", choices = unique(average_dcq()$cell))
        updateSelectInput(session, "columnInput", choices = grep("^fc_ddcq", names(average_dcq()), value = TRUE))
        } else {
          updateSelectInput(session, "sampleInput", choices=character(0), selected = character(0))  # Clear the sampleInput choices
          updateSelectInput(session, "columnInput", choices=character(0), selected = character(0))
        }
      }
  })
  
  output$ddcqMessage <- renderUI({
    # Check if 'ddcq_stats' is selected and data is not saved yet
    if (input$select_dcq_or_ddcq_stats == "ddcq_stats" && !values$ddcqDataSaved) {
      # Return a UI element with the message
      tagList(
        HTML('<h5>Please go to the 2<sup>-(∆∆Cq)</sup> Calculations tab to create your ∆∆Cq dataset.</h5>'),
        tags$p("You need to save your ∆∆Cq dataset before proceeding.")
      )
    } else {
      # Return NULL or an empty UI element if conditions are not met
      return()
    }
  })
  
stats_data <- reactive({
  if (input$select_dcq_or_ddcq_stats == "dcq_stats") {
    wrangled_data()
  } else if (input$select_dcq_or_ddcq_stats == "ddcq_stats" && values$ddcqDataSaved) {
    average_dcq()
  } else{
    return(NULL)
  }
})
  
# Observe changes in the selection between dcq and ddcq
observeEvent(input$select_dcq_or_ddcq_stats, {
  # Reset the 'sample_size' checkbox
  updateCheckboxInput(session, "sample_size", value = FALSE)
  
  # Clear all selections in 'normality_test' checkbox group
  updateCheckboxGroupInput(session, "normality_test", selected = character(0))
  
  # Reset the 'variance' checkbox
  updateCheckboxInput(session, "variance", value = FALSE)
  
  # Reset the 'log_transform' checkbox
  updateCheckboxInput(session, "log_transform", value = FALSE)
  
  # Clear the selection in 'group_comparison' radio buttons
  updateRadioButtons(session, "group_comparison", selected = character(0))
})

  # Dynamically render the heading based on the checkbox
  output$sampleSizeHeading <- renderUI({
    if (input$sample_size) {  # Check if the checkbox is ticked
      h4(HTML("<b>Sample Size</b>"))  # Display the heading
    }
  })
  
  # Reactive expression to calculate the count of non-NA values for each sample
  sampleCounts <- reactive({
    req(input$sampleInput, input$columnInput)  # Ensure the inputs are not NULL
    # Filter the data based on selected samples
    selected_data <- stats_data()[stats_data()$cell %in% input$sampleInput, ]
    # Calculate the count of non-NA entries for the selected fc_dcq_ column for each sample
    counts <- tapply(selected_data[[input$columnInput]], selected_data$cell, function(x) sum(!is.na(x)))
    # Return only the counts for the samples selected by the user
    counts[names(counts) %in% input$sampleInput]
  })
  
  # Reactive expression to create a table with sample sizes
  sampleSizeTable <- reactive({
    counts <- sampleCounts()  # Get the sample counts
    if (is.null(counts) || length(counts) == 0) {
      return(data.frame(Sample = character(0), N = integer(0)))  # Return an empty dataframe if there are no counts
    }
    # Create the dataframe with the sample names and their corresponding counts
    data.frame(Sample = names(counts), N = counts, stringsAsFactors = FALSE, row.names = NULL)
  })
  
  # Render the table output using the sample size table, only when checkbox is selected
  output$nTable <- renderDataTable({
    if (input$sample_size) {  # Check if the checkbox is selected
      datatable <- sampleSizeTable()  # Get the sample size table
      if (nrow(datatable) == 0) {
        return(data.frame(Sample = "No data available", N = NA))  # Display message if no data
      }
      datatable  # Render the datatable
    } else {
      return(NULL)  # If checkbox is not selected, return NULL (don't render the table)
    }
  }, options = list(pageLength = 5))
  
  #shapiro-wilk
  # Dynamically render the heading based on the checkbox
  output$normalityHeading <- renderUI({
    if (!is.null(input$normality_test) && any(input$normality_test %in% c("shapiro", "ks", "qqplot", "density"))) {      
      h4(HTML("<b>Normality Test</b>"))  # Display the heading
    }
  })
  
  shapiro_data_reactive <- reactive({
    req(input$select_dcq_or_ddcq_stats, input$sampleInput, input$columnInput, stats_data()) 
      # Initial data filtering and selection
      data <- stats_data() %>%
        filter(cell %in% input$sampleInput) %>%
        dplyr::select(cell, !!as.symbol(input$columnInput)) %>%
        filter(!is.na(!!as.symbol(input$columnInput))) %>%
        droplevels()
      # Apply log10 transformation if the checkbox is checked
      if (input$log_transform == TRUE) {
        data <- data %>%
          mutate(!!as.symbol(input$columnInput) := log10(!!as.symbol(input$columnInput)))
      }
      data
  }) 
  
  
  # Reactive expression for performing the Shapiro-Wilk test
  test_results_shapiro <- reactive({
    req(input$normality_test == "shapiro") # Proceed only if Shapiro-Wilk is selected
    data_for_shapiro <- shapiro_data_reactive() # Use the reactive filtered data
    
    # Split the data by the 'cell' factor and perform Shapiro-Wilk test for each group
    shapiro_col <- input$columnInput
    grouped_data <- split(data_for_shapiro[[shapiro_col]], data_for_shapiro$cell)
    
    # Initialize an empty data frame to store the results
    results_df <- data.frame(
      Group = character(),
      W = numeric(),
      P_value = numeric(),
      Passed_normality_test = logical(),
      P_value_summary = character(),
      stringsAsFactors = FALSE
    )
    
    # Loop through the list of groups and perform the Shapiro-Wilk test
    for (group_name in names(grouped_data)) {
      test_result <- tryCatch({
        shapiro.test(grouped_data[[group_name]])
      }, error = function(e) {
        return(list(statistic = NA, p.value = NA))
      })
      
      # Determine if the group passes the normality test based on the p-value
      passed_test <- test_result$p.value > 0.05
      # Determine the p-value summary
      p_value_summary <- ifelse(test_result$p.value < 0.001, "***", 
                                ifelse(test_result$p.value < 0.01, "**", 
                                       ifelse(test_result$p.value > 0.05, "ns", "*")))
      
      # Add the results to the data frame
      results_df <- rbind(results_df, data.frame(
        Group = group_name,
        W = test_result$statistic,
        P_value = test_result$p.value,
        Passed_normality_test = ifelse(passed_test, "Yes", "No"),
        P_value_summary = p_value_summary,
        stringsAsFactors = FALSE
      ))
      
    }
    rownames(results_df) <- NULL
    # Return the results data frame
    return(results_df)
  })
  
  observe({
    results_shapiro <- test_results_shapiro() # Assumes this reactive expression exists
    
    results_shapiro <- results_shapiro %>% 
      rename("P Value" = P_value, "Passed Normality Test?" = Passed_normality_test, "P Value Summary" = P_value_summary)
    # Dynamically create or remove the table based on selection
    output$normalityTableUI <- renderUI({
      if("shapiro" %in% input$normality_test) {
        # Check if there's an error in the results and display it, otherwise display the table
        if (is.list(results_shapiro) && !is.null(results_shapiro$error)) {
          # Output the error message if present
          tableOutput("shapiroError")
        } else {
          #Construct and render the results table
          dataTableOutput("normalityTable")
        }
      }
    })
    
    # Conditionally render the error message or the results table
    output$shapiroError <- renderTable({
      if(is.list(results_shapiro) && !is.null(results_shapiro$error)) {
        matrix(results_shapiro$error, nrow = 1)
      }
    })
    
    output$normalityTable <- renderDataTable({
      if(!is.null(results_shapiro) && is.list(results_shapiro) && is.null(results_shapiro$error)) {
        results_shapiro
      }
    })
  })
  
  # Define a reactive expression that generates the QQ plot
  qqPlot_reactive <- reactive({
    req("qqplot" %in% input$normality_test, !is.null(input$columnInput))
    qqplot_data <- shapiro_data_reactive()
    
    # Generate the plot
    ggplot(qqplot_data, aes(sample = !!as.symbol(input$columnInput))) +
      geom_qq() + geom_qq_line() +
      facet_wrap(~cell, scales = "free_y") +
      labs(title = "QQ Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_Marnie
    
  })
  
  # Use the reactive expression for rendering the plot in the Shiny app
  output$qqPlot <- renderPlot({
    req(qqPlot_reactive())  # Make sure the reactive expression has been evaluated
    qqPlot_reactive()  # Return the plot created by the reactive expression
  })
  
  densityPlot_reactive <- reactive({
    req(input$normality_test == "density", input$columnInput)
    density_data <- shapiro_data_reactive()
    p <- density_data %>% 
      ggplot(aes(x = !!as.symbol(input$columnInput))) +
      geom_density() +
      facet_wrap(~cell, scales = "free_y") +
      labs(title = "Density Plot", x = "Value", y = "Density") +
      theme_Marnie
    return(p)
  })
  
  output$densityPlot <- renderPlot({
    req(densityPlot_reactive())
    densityPlot_reactive()
  })
  
  output$qqPlotUI <- renderUI({
    if (!is.null(input$normality_test) && length(input$normality_test) > 0 && "qqplot" %in% input$normality_test) {
      plotOutput("qqPlot")
    }
  })
  
  output$densityPlotUI <- renderUI({
    if (!is.null(input$normality_test) && length(input$normality_test) > 0 && "density" %in% input$normality_test) {
      plotOutput("densityPlot")
    }
  })
  
  output$leveneHeading <- renderUI({
    if (input$variance == TRUE ) {      
      h4(HTML("<b>Homogeneity of Variance Test</b>"))  # Display the heading
    }
  })
  levene_reactive <- reactive({
    req(input$variance == TRUE, input$columnInput)
    levene_test_data <- shapiro_data_reactive()
    test_result <- leveneTest(levene_test_data[[input$columnInput]] ~ levene_test_data$cell)
    # Extracting values from the test_result
    df_group <- test_result$Df[1] # Degrees of freedom for group
    df_error <- test_result$Df[2] # Degrees of freedom for error/residuals
    f_value <- test_result$`F value`[1] # Correct access for F value
    p_value <- test_result$`Pr(>F)`[1]
    # Construct summary data frame
    summary_df <- data.frame(
      DF_Group = df_group,
      DF_Error = df_error,
      F_Value = f_value,
      P_Value = p_value,
      Passed_variance_test = ifelse(p_value > 0.05, "Yes", "No"),
      P_value_summary = ifelse(p_value < 0.001, "***", 
                               ifelse(p_value < 0.01, "**", 
                                      ifelse(p_value > 0.05, "ns", "*")))
    )
    summary_df <- summary_df %>% 
      rename("df (Group)" = DF_Group, "df (Error)" = DF_Error, "F" = F_Value, 
             "P Value" = P_Value, "Passed Variance Test?" = Passed_variance_test, "P Value Summary" = P_value_summary)
    rownames(summary_df) <- ""
    # Return the new summary data frame
    summary_df
  })
  
  output$levene <- renderDataTable({
    req(levene_reactive())
    levene_reactive()
  })
  
  output$leveneUI <- renderUI({
    if(input$variance == TRUE) { # Check if the user wants to see the Levene's test results
      dataTableOutput("levene")
    }
  })
  
  comparisonResults <- reactive({
    # Ensure necessary inputs are available
    req(input$sampleInput, input$columnInput, input$group_comparison)
    data <- shapiro_data_reactive() # Assuming this returns your dataset
    num_groups <- length(unique(data$cell))
    
    if (num_groups == 2) {
      if (input$group_comparison == "parametric") {
        #perform t-test
        test_result <- performComparisonTests(test_type = "t-test", data = shapiro_data_reactive(), column_input = input$columnInput)
        test_result_df <- test_result$test_result_df
        #compact letter display
        cld_df <- performCLD(data = test_result_df, p_colname = "P Value (Two-Tailed)",  remove_NA = FALSE)
      } else {
        test_result <- performComparisonTests(test_type = "wilcox", data = shapiro_data_reactive(), column_input = input$columnInput)
        test_result_df <- test_result$test_result_df
        #compact letter display
        cld_df <- performCLD(data = test_result_df, p_colname = "P Value",  remove_NA = FALSE)
      }
      return(list(test = test_result_df, cld = cld_df))
    } else if (num_groups > 2) {
      if (input$group_comparison == "parametric") {
        test_result <- performComparisonTests(test_type = "ANOVA", data = shapiro_data_reactive(), column_input = input$columnInput)
        test_result_df <- test_result$test_result_df
        aov_result  <- test_result$aov_result
        
        if(input$postHocTest == "tukey"){
          results <- performTukeyPostHoc(aov_df = aov_result, p_adjust_method = "tukey")
          # You can then access each dataframe like this:
          post_hoc_df <- results$post_hoc_df
          
          #compact letter display
          cld_df <- results$cld_df
          cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
          
        }else if (input$postHocTest == "bonferroni"){
          try({
            # Validate conditions
            results <- performPostHoc(data = shapiro_data_reactive(), p_adjust_method = "bonferroni", input_column = input$columnInput, sample_sizes = sampleSizeTable())
            # You can then access each dataframe like this:
            post_hoc_df <- results$post_hoc_df
            
            #compact letter display
            cld_df <- results$cld_df
            cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = TRUE)
          }, silent = TRUE)
          

        }else if (input$postHocTest == "holm"){
          try({
            # Validate conditions
            results <- performPostHoc(
              data = shapiro_data_reactive(),
              p_adjust_method = "holm",
              input_column = input$columnInput,
              sample_sizes = sampleSizeTable()
            )
            # You can then access each dataframe like this:
            post_hoc_df <- results$post_hoc_df
            
            #compact letter display
            cld_df <- results$cld_df
            cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = TRUE)
          }, silent = TRUE)
          
        }else if (input$postHocTest == "bh"){
          try({
            # Validate conditions
            results <- performPostHoc(
              data = shapiro_data_reactive(),
              p_adjust_method = "BH",
              input_column = input$columnInput,
              sample_sizes = sampleSizeTable()
            )
            # You can then access each dataframe like this:
            post_hoc_df <- results$post_hoc_df
            
            #compact letter display
            cld_df <- results$cld_df
            cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = TRUE)
          }, silent = TRUE)
        }else if (input$postHocTest == "scheffe"){
          results <- performPostHoc(data = shapiro_data_reactive(), p_adjust_method = "scheffe", input_column = input$columnInput, sample_sizes = sampleSizeTable())
          # You can then access each dataframe like this:
          post_hoc_df <- results$post_hoc_df
          cld_df <- results$cld_df
          #compact letter display
          cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
        }
        
      } else {
        test_result <- performComparisonTests(test_type = "kw", data = shapiro_data_reactive(), column_input = input$columnInput)
        test_result_df <- test_result$test_result_df
        post_hoc_df <- NULL
        if(input$postHocTest == "dunn" && input$correctionMethod == "bonferroni"){
          # Validate conditions
          results <- performDunnPostHoc(
            data = shapiro_data_reactive(),
            p_adjust_method = "bonferroni",
            input_column = input$columnInput
          )
          # You can then access each dataframe like this:
          post_hoc_df <- results$post_hoc_df
          
          #compact letter display
          cld_df <- results$cld_df
          cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = TRUE)
          
        }else if(input$postHocTest == "dunn" && input$correctionMethod == "sidak"){
          # Validate conditions
          results <- performDunnPostHoc(
            data = shapiro_data_reactive(),
            p_adjust_method = "sidak",
            input_column = input$columnInput
          )
          # You can then access each dataframe like this:
          post_hoc_df <- results$post_hoc_df
          #compact letter display
          cld_df <- results$cld_df
          cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
          
        }else if(input$postHocTest == "dunn" && input$correctionMethod == "hs"){
          # Validate conditions
          results <- performDunnPostHoc(
            data = shapiro_data_reactive(),
            p_adjust_method = "hs",
            input_column = input$columnInput
          )
          # You can then access each dataframe like this:
          post_hoc_df <- results$post_hoc_df
          
          #compact letter display
          cld_df <- results$cld_df
          cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
          
        }else if(input$postHocTest == "dunn" && input$correctionMethod == "holm"){
          # Validate conditions
          results <- performDunnPostHoc(
            data = shapiro_data_reactive(),
            p_adjust_method = "holm",
            input_column = input$columnInput
          )
          # You can then access each dataframe like this:
          post_hoc_df <- results$post_hoc_df
          
          #compact letter display
          cld_df <- results$cld_df
          cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
          
        }else if(input$postHocTest == "dunn" && input$correctionMethod == "bh"){
          # Validate conditions
          results <- performDunnPostHoc(
            data = shapiro_data_reactive(),
            p_adjust_method = "bh",
            input_column = input$columnInput
          )
          # You can then access each dataframe like this:
          post_hoc_df <- results$post_hoc_df
          #compact letter display
          cld_df <- results$cld_df
          cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
          
        }else if(input$postHocTest == "dunn" && input$correctionMethod == "hochberg"){
          # Validate conditions
          results <- performDunnPostHoc(
            data = shapiro_data_reactive(),
            p_adjust_method = "hochberg",
            input_column = input$columnInput
          )
          post_hoc_df <- results$post_hoc_df
          #compact letter display
          cld_df <- results$cld_df
          cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
          
        }else if(input$postHocTest == "conover" && input$correctionMethod == "bonferroni"){
          results <-performConoverPostHoc(
            data = shapiro_data_reactive(),
            p_adjust_method = "bonferroni",
            input_column = input$columnInput
          )
          post_hoc_df <- results$post_hoc_df
          #compact letter display
          cld_df <- results$cld_df
          cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
          
        }else if(input$postHocTest == "conover" && input$correctionMethod == "sidak"){
          results <-performConoverPostHoc(
            data = shapiro_data_reactive(),
            p_adjust_method = "sidak",
            input_column = input$columnInput
          )
          post_hoc_df <- results$post_hoc_df
          #compact letter display
          cld_df <- results$cld_df
          cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
          
        }else if(input$postHocTest == "conover" && input$correctionMethod == "holm"){
          results <-performConoverPostHoc(
            data = shapiro_data_reactive(),
            p_adjust_method = "holm",
            input_column = input$columnInput
          )
          post_hoc_df <- results$post_hoc_df
          #compact letter display
          cld_df <- results$cld_df
          cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
          
        }else if(input$postHocTest == "conover" && input$correctionMethod == "bh"){
          results <-performConoverPostHoc(
            data = shapiro_data_reactive(),
            p_adjust_method = "bh",
            input_column = input$columnInput
          )
          post_hoc_df <- results$post_hoc_df
          #compact letter display
          cld_df <- results$cld_df
          cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
          
        }else if(input$postHocTest == "conover" && input$correctionMethod == "hs"){
          results <-performConoverPostHoc(
            data = shapiro_data_reactive(),
            p_adjust_method = "hs",
            input_column = input$columnInput
          )
          post_hoc_df <- results$post_hoc_df
          #compact letter display
          cld_df <- results$cld_df
          cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
          
        }else if(input$postHocTest == "conover" && input$correctionMethod == "hochberg"){
          results <-performConoverPostHoc(
            data = shapiro_data_reactive(),
            p_adjust_method = "hochberg",
            input_column = input$columnInput
          )
          post_hoc_df <- results$post_hoc_df
          #compact letter display
          cld_df <- results$cld_df
          cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
        }
        
      }
      return(list(test = test_result_df, posthoc = post_hoc_df, cld = cld_df))
    } else {
      return(NULL) # Handle the case where there are not enough groups
    }
  })
  
  # Render the dataframe using DT::renderDataTable
  output$dataTable <- renderDataTable({
    req(comparisonResults()) # Ensure the dataframe is ready
    datatable(comparisonResults()$test, options = list(pageLength = 5, autoWidth = TRUE)) 
  })
  
  # Use renderUI to dynamically generate dataTableOutput
  output$testResultTable <- renderUI({
    # Dynamically create a dataTableOutput element
    dataTableOutput("dataTable")
  })
  
  
  output$postHocTableUI <- renderUI({
    # Dynamically create a dataTableOutput element
    dataTableOutput("postHocTable")
  })
  
  output$postHocTable <- renderDataTable({
    req(comparisonResults())  # Ensure the post-hoc results are available
    if (!is.null(comparisonResults()$posthoc)) {
      datatable(comparisonResults()$posthoc, options = list(pageLength = 5, autoWidth = TRUE))
    }
  })
  
  # Render the dataframe using DT::renderDataTable
  output$cld_table <- renderDataTable({
    req(comparisonResults()) # Ensure the dataframe is ready
    datatable(comparisonResults()$cld) 
  })

  
  # Use renderUI to dynamically generate dataTableOutput
  output$cld_tableUI <- renderUI({
    # Dynamically create a dataTableOutput element
    dataTableOutput("cld_table")
  })
  
  output$comparisonsHeading <- renderUI({
    req(input$sampleInput, input$columnInput, input$group_comparison)
    data <- shapiro_data_reactive() # Assuming this returns your dataset
    
    num_groups <- length(unique(data$cell))
    
    if (num_groups == 2) {
      if (input$group_comparison == "parametric") {
        h4(HTML("<b>Independent T-Test</b>"))
      } else {
        h4(HTML("<b>Mann-Whitney U Test</b>"))
      }
    } else if (num_groups > 2) {
      if (input$group_comparison == "parametric") {
        h4(HTML("<b>One-Way ANOVA</b>"))
      } else {
        h4(HTML("<b>Kruskal-Wallis Test</b>"))
      }
    } else {
      return(NULL) # Handle the case where there are not enough groups
    }
  })
  
  # Dynamically generate UI for post hoc options based on the type of test and number of groups
  output$postHocOptions <- renderUI({
    # Ensure we have more than 2 groups for post hoc tests to make sense
    req(input$sampleInput, input$columnInput, input$group_comparison)
    data <- shapiro_data_reactive() 
    num_groups <- length(unique(data$cell))
    
    if (input$group_comparison == "parametric" && num_groups > 2) {
      # Parametric post hoc test options
      radioButtons("postHocTest", HTML("<b>Select a post hoc test for ANOVA (if <i>p</i> < 0.05):</b>"),
                   choices = list("Tukey's HSD" = "tukey",
                                  "Bonferroni Correction" = "bonferroni",
                                  "Holm Correction" = "holm",
                                  "Benjamini-Hochberg Correction" = "bh",
                                  "Scheffé's Test" = "scheffe"))
    } else if (input$group_comparison == "non_parametric" && num_groups > 2) {
      # Nonparametric post hoc test options
      radioButtons("postHocTest", HTML("<b>Select a post hoc test for Kruskal-Wallis (if <i>p</i> < 0.05):</b>"),
                   choices = list("Dunn's Test" = "dunn",
                                  "Conover-Iman Test" = "conover"))
    } else {
      return(NULL) 
    }
  })
  
  
  
  output$correctionOptions <- renderUI({
    num_groups <- length(unique(shapiro_data_reactive()$cell))
    # Ensure we have more than 2 groups for post hoc tests to make sense
    req(input$sampleInput, input$columnInput, input$group_comparison, num_groups > 2)
    if (!is.null(input$postHocTest) && (input$postHocTest == "dunn" || input$postHocTest == "conover")) {
      radioButtons("correctionMethod", HTML("<b>Select a correction method for Dunn's test:</b>"),
                   choices = list("Bonferroni" = "bonferroni",
                                  "Šidák" = "sidak",
                                  "Holm" = "holm",
                                  "Holm-Šidák" = "hs",
                                  "Benjamini-Hochberg" = "bh",
                                  "Hochberg's Step-up" = "hochberg"))
    }else{
      return(NULL)
    }
  })
  
  output$postHocHeading <- renderUI({
    req(input$sampleInput, input$columnInput, input$group_comparison, input$postHocTest)
    data <- shapiro_data_reactive() 
    num_groups <- length(unique(data$cell))
    if (input$group_comparison == "parametric" && input$postHocTest == "tukey" && num_groups > 2) {
      h4(HTML("<b>Tukey's HSD Post-hoc</b>"))
    } else if (input$group_comparison == "parametric" && input$postHocTest == "bonferroni" && num_groups > 2) {
      h4(HTML("<b>Pairwise t-test with Bonferroni adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "parametric" && input$postHocTest == "holm" && num_groups > 2) {
      h4(HTML("<b>Pairwise t-test with Holm adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "parametric" && input$postHocTest == "bh" && num_groups > 2) {
      h4(HTML("<b>Pairwise t-test with Benjamini-Hochberg adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "parametric" && input$postHocTest == "scheffe" && num_groups > 2) {
      h4(HTML("<b>Scheffé's Post-hoc</b>"))  
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "dunn" && input$correctionMethod == "bonferroni" && num_groups > 2) {
      h4(HTML("<b>Dunn's test with Bonferroni adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "dunn" && input$correctionMethod == "sidak" && num_groups > 2) {
      h4(HTML("<b>Dunn's test with Šidák adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "dunn" && input$correctionMethod == "holm" && num_groups > 2) {
      h4(HTML("<b>Dunn's test with Holm adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "dunn" && input$correctionMethod == "hs" && num_groups > 2) {
      h4(HTML("<b>Dunn's test with Holm-Šidák adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "dunn" && input$correctionMethod == "bh" && num_groups > 2) {
      h4(HTML("<b>Dunn's test with Benjamini-Hochberg adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "dunn" && input$correctionMethod == "hochberg" && num_groups > 2) {
      h4(HTML("<b>Dunn's test with Hochberg adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "conover" && input$correctionMethod == "bonferroni" && num_groups > 2) {
      h4(HTML("<b>Conover-Iman test with Bonferroni adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "conover" && input$correctionMethod == "sidak" && num_groups > 2) {
      h4(HTML("<b>Conover-Iman test with Šidák adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "conover" && input$correctionMethod == "holm" && num_groups > 2) {
      h4(HTML("<b>Conover-Iman test with Holm adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "conover" && input$correctionMethod == "hs" && num_groups > 2) {
      h4(HTML("<b>Conover-Iman test with Holm-Šidák adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "conover" && input$correctionMethod == "bh" && num_groups > 2) {
      h4(HTML("<b>Conover-Iman test with Benjamini-Hochberg adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "conover" && input$correctionMethod == "hochberg" && num_groups > 2) {
      h4(HTML("<b>Conover-Iman test with Hochberg adjustment for multiple comparisons</b>"))
    } else {
      return(NULL)
    }
  })
  
  output$cldHeading <- renderUI({
  req(comparisonResults())
    tagList(
      tags$h4(HTML("<b>Compact Letter Display</b>")),
      tags$h6(HTML("Groups with the same letter are not significantly different from each other."))
    )
  })

  

  # Graphing dcq or ddcq  
  # Define a reactive expression to switch between datasets
  dcq_or_ddcq <- reactive({
    if (input$select_dcq_or_ddcq == "dcq") {
      # If 'dcq' is selected, return wrangled_data()
      return(wrangled_data())

    } else {
      # If 'ddcq' is selected, return average_dcq()
      return(average_dcq())
    }
  })
  
  observeEvent(input$select_dcq_or_ddcq,{
    if (input$select_dcq_or_ddcq == "dcq") {
      # Render the dynamic selectInput for choosing condition
      output$condition_selector <- renderUI({
        req(wrangled_data())  # Ensure data is available

        # Generate selectInput for choosing the condition dynamically
        selectInput("selected_condition", "Select Samples", choices = unique(wrangled_data()$cell),
                    multiple = TRUE)
      })
      
      # Render the dynamic selectInput for choosing the column
      output$column_selector <- renderUI({
        req(wrangled_data())  # Ensure data is available
        
        # Filter column names to include only those starting with "fc_dcq"
        fc_dcq_columns <- grep("^fc_dcq", colnames(wrangled_data()), value = TRUE)
        
        # Generate selectInput for choosing the column dynamically
        selectInput("fc_dcq_column", "Select Gene", choices = fc_dcq_columns)
      })
    } else {
      # Hide the UI elements if ddcq is selected
      output$condition_selector <- renderUI(NULL)
      output$column_selector <- renderUI(NULL)
    }
  })
  
  
  
  observeEvent(input$select_dcq_or_ddcq, {
    # Check if 'ddcq' is selected
    if (input$select_dcq_or_ddcq == "ddcq") {
      # Display the gene selection message
      output$selected_gene_ui <- renderUI({
        textOutput("selected_gene_message")
      })
    } else {
      # Hide the message when 'dcq' is selected or for any other condition
      output$selected_gene_ui <- renderUI({})
    }
  })
  
  # Define the text output for displaying the selected gene
  output$selected_gene_message <- renderText({
    req(input$select_gene)  # Ensure there is a selection
    # Use gsub to remove "dcq_" from the selected gene's name
    selected_gene_cleaned <- gsub("^dcq_", "", input$select_gene)
    paste("You are currently graphing gene:", selected_gene_cleaned)
  })
  
  output$dynamic_y_label_input <- renderUI({
    # Initialize variable to store cleaned gene name
    selected_gene_cleaned <- ""
    
    # Determine which input to use based on the condition selected
    if (input$select_dcq_or_ddcq == "dcq") {
      # Ensure the fc_dcq_column input is used for dcq condition
      req(input$fc_dcq_column)  # Ensure there is a selection for the dcq condition
      selected_gene_cleaned <- gsub("^fc_dcq_", "", input$fc_dcq_column)  # Clean the gene name for dcq
    } else if (input$select_dcq_or_ddcq == "ddcq") {
      # Assume there's another input mechanism for ddcq or use a default value
      req(input$select_gene)  # Placeholder, adjust as necessary for ddcq
      selected_gene_cleaned <- gsub("^dcq_", "", input$select_gene)  # Clean the gene name for ddcq
    }
    
    # Determine the placeholder text based on the selection
    placeholder_text <- if (input$select_dcq_or_ddcq == "dcq") {
      paste0("Relative *", selected_gene_cleaned, "* mRNA (2<sup>-ΔCq</sup>)")
    } else if (input$select_dcq_or_ddcq == "ddcq") {
      paste0("Fold change *", selected_gene_cleaned, "* mRNA (2<sup>-ΔΔCq</sup>)")
    } else {
      "Enter Y-axis Label"  # Default text if neither is selected
    }
    
    # Generate the text input with the dynamic placeholder
    textInput("y_label", "Enter Y-axis Label. Markup is accepted.", value = placeholder_text)
  })
  
  
  # Get the color scheme based on user input
  color_schemes <- getColourSchemes()
  
  # Dynamic generation of text inputs based on positions
  output$x_axis_labels <- renderUI({
    positions <- if (!is.null(input$x_axis_positions)) {
      unlist(strsplit(input$x_axis_positions, ","))
    } else {
      NULL
    }
    
    
    if (!is.null(positions) && length(positions) > 0) {
      lapply(positions, function(pos) {
        textInput(inputId = paste0("label_", pos),
                  label = paste("Label for", pos),
                  value = pos)  # Initial value can be set to the position itself or an empty string
      })
    }
  })
  
  
  # Reactive function to get user-entered labels
  user_labels <- reactive({
    positions <- if (!is.null(input$x_axis_positions)) {
      unlist(strsplit(input$x_axis_positions, ","))
    } else {
      NULL
    }
    
    if (!is.null(positions) && length(positions) > 0) {
      sapply(positions, function(pos) {
        input[[paste0("label_", pos)]]
      })
    }
  })
  
  observeEvent(input$labels_positions, {
    # Parse labels if user entered them
    filtered_labels <- if (!is.null(input$labels_positions)) {
      unlist(strsplit(input$labels_positions, ","))
    } else {
      NULL
    }
    
    # Update the UI with text boxes for custom labels
    label_textboxes <- lapply(filtered_labels, function(label) {
      textInput(paste0("label_", label), label, label)
    })
    
    # Render UI for text boxes
    output$labels_textboxes <- renderUI({
      label_textboxes
    })
  })
  
  mean_sd <- function(x) {
    n <- sum(!is.na(x)) # Number of non-NA values
    if (n > 1) { # Can only calculate sd if n > 1
      sd_x <- sd(x, na.rm = TRUE)
      mean_x <- mean(x, na.rm = TRUE)
      return(data.frame(y = mean_x, ymin = mean_x - sd_x, ymax = mean_x + sd_x))
    } else {
      return(data.frame(y = NA, ymin = NA, ymax = NA)) # Return NA if not enough data
    }
  }
  
  se <- function(x) {
    n <- sum(!is.na(x))
    if (n > 1) {
      sd_x <- sd(x, na.rm = TRUE) / sqrt(n)
      mean_x <- mean(x, na.rm = TRUE)
      return(data.frame(y = mean_x, ymin = mean_x - sd_x, ymax = mean_x + sd_x))
    } else {
      return(data.frame(y = NA, ymin = NA, ymax = NA))
    }
  }
  
  fc_dcq_columns_reactive <- reactive({
    req(wrangled_data())  # Ensure data is available
    grep("^fc_dcq", colnames(wrangled_data()), value = TRUE)
  })
  
  # Now, you can access `fc_dcq_columns_reactive()` as a reactive value.
  
  
  output$plot <- renderPlot({
    req(input$select_dcq_or_ddcq, input$y_label, input$x_label, input$fc_dcq_column)
    set.seed(input$seed_input)
    
    if(input$select_dcq_or_ddcq == "dcq"){
      filtered_data2 <- dcq_or_ddcq() %>%
        filter(cell %in% input$selected_condition)
      filtered_rep_avg_data2 <- rep_avg_data() %>%
        filter(cell %in% input$selected_condition)
      #If filtered_rep_avg_data2 has column fc_avg, rename to input$selected_condition
      #This is needed if only one gene is present in the dataset apart from the housekeepers
          if("fc_avg" %in% colnames(filtered_rep_avg_data2)){
            filtered_rep_avg_data2 <- filtered_rep_avg_data2 %>%
              rename(!!input$fc_dcq_column := fc_avg)
          }
      # Determine the x aesthetic based on the number of selected conditions
      x_aes <- if (length(input$selected_condition) >= 2) {
        sym("cell")
      } else {
        sym("group")
      }

      # Specify the y_aes based on user input
      y_aes <- sym(input$fc_dcq_column)
      y_aes_avg <- sym(input$fc_dcq_column)
      
      positions <- if (length(input$selected_condition) >= 2) {
        # Parse positions if user entered them, or use unique values from "cell" column
        if (!is.null(input$x_axis_positions)) {
          unlist(strsplit(input$x_axis_positions, ","))
        } else {
          unique(filtered_data2$cell)
        }
      } else {
        unlist(strsplit(input$x_axis_positions, ","))
      }
      
    }else if(input$select_dcq_or_ddcq == "ddcq"){
      filtered_data2 <- dcq_or_ddcq()
      filtered_rep_avg_data2 <- rep_avg_data_ddcq()
      # Determine the x aesthetic based on the number of selected conditions
      x_aes <- sym("cell")
      # Specify the y_aes based on user input
      y_aes <- sym("fc_ddcq")
      y_aes_avg <- sym("mean_fc_ddcq")

      positions <- if (length(input$select_samples) >= 2) {
        # Parse positions if user entered them, or use unique values from "cell" column
        if (!is.null(input$x_axis_positions)) {
          unlist(strsplit(input$x_axis_positions, ","))
        } else {
          unique(filtered_data2$group)
        }
      } else {
        unlist(strsplit(input$x_axis_positions, ","))
      }
    }
    
    
    # Check if x-axis categories are available
    if (is.null(input$x_axis_positions) || input$x_axis_positions == "") {
      validate(
        need(FALSE, "Please enter x-axis categories to build the graph.")
      )
    }
    
    # if(output$column_selector == ) {
    #   validate(
    #     need(FALSE, "Please select another gene that has values for the selected groups/conditions.")
    #   )
    # }
    # Get the color scheme based on user input
    color_scheme <- input$color_scheme_select
    colors <- if (color_scheme == "custom") {
      # If the user selects "Custom," use the custom colors defined earlier
      color_schemes$custom
    } else if (color_scheme %in% names(color_schemes)) {
      # If the user selects one of the predefined schemes, use the corresponding colors
      color_schemes[[color_scheme]]
    } else {
      # If none of the above, use a default set of colors
      c("#ffb000", "#648fff", "#dc267f", "#785ef0", "#00359c", "#fe6100")
    }
    
    
    # Check if the colour palette is smaller than the number of positions entered by the user
    if (length(colors) < length(positions)) {
      validate(
        need(FALSE, "The selected colour palette is smaller than the number of x-axis groups.")
      )
    }
    # Define x-axis theme based on checkbox
    x_axis_theme <- if (input$rotate_labels) {
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
    }
    
    shapes_reactive <- reactive({
      if (input$change_shapes) {
        # New order when checkbox is selected
        setNames(c(16, 21, 17, 24, 15, 22, 18, 23, 4, 3, 8, 10, 9, 11, 12, 13, 14), positions)
      } else {
        # Default order
        setNames(c(16, 17, 15, 18, 4, 3, 8, 10, 9, 11, 12, 13, 14, 21, 22, 23, 24, 25), positions)
      }
    })
    
    
    
    # Determine which function to use based on user input
    error_fun <- if(input$error_type == "sd") {
      mean_sd
    } else {
      mean_se # Assuming mean_se is defined or available
    }
    
    # Calculate the y-limits based on the error function
    summary_stats <- error_fun(filtered_data2[[y_aes]])
    min_ymin <- min(summary_stats$ymin, na.rm = TRUE)
    max_ymax <- max(summary_stats$ymax, na.rm = TRUE)


    # Check if ymin is below 0 and adjust y_limits accordingly
    y_limits <- if (min_ymin < 0) {
      lower_limit <- min_ymin * 1.1 # Give a 10% buffer below the minimum ymin
      c(lower_limit, NA)
    } else {
      c(0, NA) # If ymin is not below 0, start the y-axis at 0
    }
    #Create your plot using ggplot2 with the selected dataset
    if (input$plot_type == "column"){
      if (input$fill_color_toggle == "color"){
        plot <- ggplot(filtered_data2, aes(x = !!x_aes, y = !!y_aes)) +
          geom_bar(data = filtered_rep_avg_data2, aes(x = !!x_aes, y = !!y_aes_avg, color = !!x_aes), stat = "identity", inherit.aes = FALSE, fill = "white", size = 1, width = 0.7, show.legend = FALSE, na.rm = TRUE) +
          stat_summary(fun.data = error_fun, geom = "errorbar", width = input$errorbar_width, aes(color = !!x_aes), linewidth = input$errorbar_thickness, na.rm = TRUE,  show.legend = FALSE) +
          geom_beeswarm(size = input$dot_size, method = "hex", cex = input$dot_spacing, na.rm = TRUE, aes(color = !!x_aes),  show.legend = FALSE) +
          labs(y = input$y_label, x = input$x_label) +
          scale_color_manual(values = setNames(colors, positions)) +  # Set custom colors using values from input$color_scheme_select
          theme_Marnie +
          scale_x_discrete(limits = positions, labels = user_labels()) +
          x_axis_theme 
      }else if (input$fill_color_toggle == "fill"){
        plot <- ggplot(filtered_data2, aes(x = !!x_aes, y = !!y_aes)) +
          geom_bar(data = filtered_rep_avg_data2, aes(x = !!x_aes, y = !!y_aes_avg, fill = !!x_aes), stat = "identity", inherit.aes = FALSE, color = "black", size = 1, width = 0.7, show.legend = FALSE, na.rm = TRUE) +
          stat_summary(fun.data = error_fun, geom = "errorbar", width = input$errorbar_width, color = "black", linewidth = input$errorbar_thickness, na.rm = TRUE,  show.legend = FALSE) +
          geom_beeswarm(size = input$dot_size, method = "hex", cex = input$dot_spacing, na.rm = TRUE, aes(fill = !!x_aes),  show.legend = FALSE) +
          labs(y = input$y_label, x = input$x_label) +
          scale_fill_manual(values = setNames(colors, positions)) +  # Set custom colors using values from input$color_scheme_select
          theme_Marnie +
          scale_x_discrete(limits = positions, labels = user_labels()) +
          x_axis_theme
      }
    }else if (input$plot_type == "dot") {
      # Dot plot
      plot <- ggplot(filtered_data2, aes(x = !!x_aes, y = !!y_aes)) +
        geom_point(size = input$point_size, na.rm = TRUE, aes(color = !!x_aes, shape = !!x_aes),
                   show.legend = FALSE, stroke = input$stroke_thickness, position = position_jitter(width = input$jitter_amount)) +
        stat_summary(fun.data = error_fun, geom = "errorbar", width = input$error_bar_width, colour = "black", linewidth = input$error_bar_thickness, na.rm = TRUE,  show.legend = FALSE) +
        stat_summary(data = filtered_rep_avg_data2, aes(x = !!x_aes, y = !!y_aes_avg), inherit.aes = FALSE,
                     fun = mean, geom = "crossbar", width = input$average_line_width, linewidth = input$average_line_thickness, show.legend = FALSE, colour = "black") +  # Add average line for each column x
        labs(y = input$y_label, x = input$x_label) +
        scale_color_manual(values = setNames(colors, positions)) +
        scale_shape_manual(values = shapes_reactive()) +
        theme_Marnie +
        scale_x_discrete(limits = positions, labels = user_labels()) +
        x_axis_theme
    }
    
    if (input$start_at_zero) {
      plot <- plot +
        scale_y_continuous(expand=expansion(mult=c(0,0.1)), limits = c(0,NA))
    }
    # Set font based on user selection
    font_family <- input$font_selector
    plot <- plot + theme(text = element_text(family = font_family))
    plot <- plot + theme(axis.title.y = element_markdown())
    
    
    # Customize x-axis and y-axis label font size using numeric input
    axis_label_theme <- theme()
    
    if (input$x_axis_title_font_size != 14) {
      axis_label_theme <- axis_label_theme + theme(axis.title.x = element_text(size = input$x_axis_title_font_size))
    }
    
    if (input$y_axis_title_font_size != 14) {
      axis_label_theme <- axis_label_theme + theme(axis.title.y = element_text(size = input$y_axis_title_font_size))
    }
    
    # Apply axis label theme to the plot
    plot <- plot + axis_label_theme
    
    
    
    # Customize x-axis and y-axis label font size using numeric input
    axis_label_theme2 <- theme()
    
    if (input$x_axis_label_font_size != 12) {
      axis_label_theme2 <- axis_label_theme2 + theme(axis.text.x = element_text(size = input$x_axis_label_font_size))
    }
    
    if (input$y_axis_label_font_size != 12) {
      axis_label_theme2 <- axis_label_theme2 + theme(axis.text.y = element_text(size = input$y_axis_label_font_size))
    }
    
    # Apply axis label theme to the plot
    plot <- plot + axis_label_theme2
    
    if(input$add_significance == "asterix"){
      num_groups <- length(unique(shapiro_data_reactive()$cell))
      if(num_groups > 2){
        comparisonResults_posthoc_renamed <- comparisonResults()$posthoc %>%
          rename(p.signif = `P Value Summary`)
        plot <- plot + stat_pvalue_manual(comparisonResults_posthoc_renamed, label = "p.signif", y.position = input$yPos, label.size = input$sigSize, bracket.size = input$bracketSize, 
                                          step.increase = input$stepIncrease, hide.ns = input$hideNS, tip.length = input$tipLength, na.rm = TRUE, inherit.aes= FALSE)
      }else{
        comparisonResults_renamed <- comparisonResults()$test %>%
          rename(p.signif = `P Value Summary`)
        plot <- plot + stat_pvalue_manual(comparisonResults_renamed, label = "p.signif", y.position = input$yPos, label.size = input$sigSize, bracket.size = input$bracketSize, 
                                          step.increase = input$stepIncrease, hide.ns = input$hideNS, tip.length = input$tipLength, na.rm = TRUE, inherit.aes= FALSE)
      }
      
    }else if(input$add_significance == "cld"){
      # Assuming comparisonResults()$cld_df has columns 'group' and 'Letters'
      cld_data <- comparisonResults()$cld
      # Transform cld_data to have group1 and group2 columns, both containing the same group values
      cld_data <- cld_data %>%
        mutate(group1 = Group, 
               group2 = Group)
      
      plot <- plot + stat_pvalue_manual(cld_data, label = "Letters", y.position = input$yPos, label.size = input$sigSize, bracket.size = input$bracketSize, 
                                        step.increase = input$stepIncrease, hide.ns = input$hideNS, tip.length = input$tipLength, na.rm = TRUE, inherit.aes= FALSE)
    }else if(input$add_significance == "pval"){
      num_groups <- length(unique(shapiro_data_reactive()$cell))
      if(num_groups > 2){
      plot_data <- comparisonResults()$posthoc %>%
        rename(p.signif = `Adjusted P Value`)
      
      # Further filter if hideNS is TRUE
      if(input$hideNS){
        plot_data <- plot_data %>%
          # Ensure p.signif is numeric for comparison, handling potential character data
          mutate(p.signif = as.numeric(as.character(p.signif))) %>%
          filter(p.signif <= 0.05)
      }
      # Check if plot_data is empty after filtering
      if(nrow(plot_data) == 0){
        plot <- plot
      }else{

        # Then, format p-values according to user preferences
        formatted_pvalues <- lapply(plot_data$`p.signif`, function(p) {
          # Format the p-value with the specified number of decimal places
          formatted_p <- sprintf(paste0("%.", input$pValueDecimals, "f"), p)
          
          # Check and remove the leading zero if required
          if(input$remove0) {
            formatted_p <- sub("^0\\.", ".", formatted_p)
          }
          
          # Determine the prefix based on user selection
          prefix <- ifelse(input$pValuePrefix == "P = ", "P = ", "")
          
          # Construct the final string with prefix and potentially modified p-value
          final_string <- paste0(prefix, formatted_p)
          
          return(final_string)
        })
        
        

        # Update plot_data with formatted p-values
        plot_data$`p.signif` <- unlist(formatted_pvalues)
        # Finally, add the formatted p-values to the plot
        plot <- plot + stat_pvalue_manual(plot_data, label = "p.signif", 
                                          y.position = input$yPos, label.size = input$sigSize, 
                                          bracket.size = input$bracketSize, 
                                          step.increase = input$stepIncrease, hide.ns = input$hideNS, 
                                          tip.length = input$tipLength, na.rm = TRUE, 
                                          inherit.aes= FALSE)
      }
      }else{
        plot_data <- comparisonResults()$test
        if("P Value" %in% names(plot_data)) {
          plot_data <- plot_data %>% 
            rename(p.signif = `P Value`)
        } else if("P Value (Two-Tailed)" %in% names(plot_data)) {
          plot_data <- plot_data %>%
            rename(p.signif = `P Value (Two-Tailed)`)
        }
        # Ensure the column is numeric for further operations
        plot_data <- plot_data %>%
          mutate(p.signif = as.numeric(as.character(p.signif)))
        
        # Then, format p-values according to user preferences
        formatted_pvalues <- lapply(plot_data$`p.signif`, function(p) {
          # Format the p-value with the specified number of decimal places
          formatted_p <- sprintf(paste0("%.", input$pValueDecimals, "f"), p)
          
          # Check and remove the leading zero if required
          if(input$remove0) {
            formatted_p <- sub("^0\\.", ".", formatted_p)
          }
          
          # Determine the prefix based on user selection
          prefix <- ifelse(input$pValuePrefix == "P = ", "P = ", "")
          
          # Construct the final string with prefix and potentially modified p-value
          final_string <- paste0(prefix, formatted_p)
          
          return(final_string)
        })

        # Update plot_data with formatted p-values
        plot_data$`p.signif` <- unlist(formatted_pvalues)
        # Finally, add the formatted p-values to the plot
        plot <- plot + stat_pvalue_manual(plot_data, label = "p.signif", 
                                          y.position = input$yPos, label.size = input$sigSize, 
                                          bracket.size = input$bracketSize, 
                                          step.increase = input$stepIncrease, hide.ns = input$hideNS, 
                                          tip.length = input$tipLength, na.rm = TRUE, 
                                          inherit.aes= FALSE)
      }
    }else if (input$add_significance == "none"){
      plot <- plot
    }
    
    # Set the plot size based on the height of the plot
    shinyjs::runjs(paste0('$("#download-container").height($("#plot").height());'))
    # Print the plot
    print(plot)
    
  },
  width = function() {
    input$width * 100  # Adjust the multiplier as needed
  },
  height = function() {
    input$height * 100  # Adjust the multiplier as needed
    
  })
  # Add more layers or customization as needed
  # Assuming shapiro_data_reactive() returns your dataset
  num_groups <- reactive({
    length(unique(shapiro_data_reactive()$cell))
  })
  
  output$sigUI <- renderUI({
    if(input$add_significance == "asterix" || input$add_significance == "cld" || input$add_significance == "pval"){
      tagList(
        fluidRow(
          column(12, numericInput("yPos", "Y position", value = 1))
        ),
        fluidRow(
          column(6, numericInput("sigSize", "Label Size", min = 0, max = 20, value = 5)),
          if(input$add_significance != "cld") {
            column(6, numericInput("bracketSize", "Bracket Thickness", min = 0, max = 10, value = 0.8, step = 0.1))
          }
        ),
        if(input$add_significance != "cld" && num_groups() > 2) {
          fluidRow(
            column(6, numericInput("stepIncrease", "Step Increase", min = 0, max = 20, value = 0.1, step = 0.1)),
            column(6, checkboxInput("hideNS", "Hide ns", value = FALSE))
          )
        },
        if(input$add_significance == "pval"){
          tagList(
            fluidRow(
              column(6, selectInput("pValuePrefix", "P-value Prefix", choices = c("None", "P = "))),
              column(6, numericInput("pValueDecimals", "Decimal Places for P-value", value = 2, min = 0, max = 10)),
            ),
            fluidRow(
              column(6, checkboxInput("remove0", "Remove leading zero from P-value", value = FALSE))
            )
          )
        },
        # Tip Length is applicable for all scenarios except "cld"
        if(input$add_significance != "cld") {
          fluidRow(
            column(12, numericInput("tipLength", "Tip Length", min = 0, max = 20, value = 0.02, step = 0.01))
          )
        }
      )
    }
  })
  

  #save gene name for naming of saved graph file
  selected_gene_name <- reactive({
    # Check if 'ddcq' is selected
    if (input$select_dcq_or_ddcq == "ddcq") {
      # Assume `input$select_gene` holds the gene name for 'ddcq'
      # Clean the gene name if needed
      clean_gene_name <- gsub("^dcq_", "", input$select_gene)
    } else {
      # For 'dcq' and other conditions, return the selected column from `fc_dcq_column`
      clean_gene_name <- gsub("^fc_dcq_", "", input$fc_dcq_column)
    }
    return(clean_gene_name)
  })
  
  #download graph with dynamic names, formats etc
  output$downloadGraph <- downloadHandler(
    filename = function() {
      # Call the reactive expression to get the current gene name
      current_gene <- selected_gene_name()
      
      # Ensure there's a default value for the gene name
      gene_name <- ifelse(is.null(current_gene) || current_gene == "", "Gene", current_gene)
      
      # Generate the filename
      paste("Graph_", gene_name, "_", Sys.Date(), ".", input$file_format, sep = "")
    },
    content = function(file) {
      # Save the plot as before
      ggsave(file, plot = last_plot(), device = input$file_format, dpi = input$dpi, width = input$width, height = input$height)
    }
  )
#STATISTICS REPORT
  #save gene name for statistics report.
  selected_gene_name_stats <- reactive({
    # Determine which input to use based on the condition selected
    if (input$select_dcq_or_ddcq_stats == "dcq_stats") {
      selected_gene_cleaned <- gsub("^fc_dcq_", "", input$columnInput)  # Clean the gene name for dcq
    } else if (input$select_dcq_or_ddcq_stats == "ddcq_stats") {
      selected_gene_cleaned <- gsub("^dcq_", "", input$select_gene)  # Clean the gene name for ddcq
    }
    return(selected_gene_cleaned)
  })
  
  #names for group comparisons
  testName <- reactive({
    req(input$sampleInput, input$columnInput, input$group_comparison)
    data <- shapiro_data_reactive() # Assuming this returns your dataset
    
    num_groups <- length(unique(data$cell))
    
    if (num_groups == 2) {
      if (input$group_comparison == "parametric") {
        "Independent T-Test"
      } else {
        "Mann-Whitney U Test"
      }
    } else if (num_groups > 2) {
      if (input$group_comparison == "parametric") {
        "One-Way ANOVA"
      } else {
        "Kruskal-Wallis Test"
      }
    } else {
      "Not enough groups" # Default message or handle as required
    }
  })
  
  # Define named vectors for the mappings
  fullPostHocTests <- c(tukey = "Tukey's HSD", 
                        dunn = "Dunn's", 
                        conover = "Conover-Iman"
                        # Add other mappings as necessary
  )
  
  fullCorrectionMethods <- c(bonferroni = "Bonferroni",
                             sidak = "Šidák",
                             holm = "Holm",
                             hs = "Holm-Šidák",
                             bh = "Benjamini-Hochberg",
                             hochberg = "Hochberg"
  )
  
  postHocTestDescription <- reactive({
    req(input$sampleInput, input$columnInput, input$group_comparison)
    data <- shapiro_data_reactive() 
    num_groups <- length(unique(data$cell))
    

    # Logic to determine the test description based on the inputs
    if (num_groups > 2) { # Ensuring there are more than 2 groups
      if (input$group_comparison == "parametric") {
        switch(input$postHocTest,
               "tukey" = "You selected a Tukey's HSD Post-hoc test.",
               "bonferroni" = "You selected a Pairwise t-test with Bonferroni adjustment.",
               "holm" = "You selected a Pairwise t-test with Holm adjustment.",
               "bh" = "You selected a Pairwise t-test with Benjamini-Hochberg adjustment.",
               "scheffe" = "You selected a Scheffé's Post-hoc test.",
               # Add other parametric tests as needed
               "Not a valid parametric post-hoc test"
        )
      } else if (input$group_comparison == "non_parametric") {
        postHocTestFullName <- fullPostHocTests[input$postHocTest]
        correctionMethodFullName <- fullCorrectionMethods[input$correctionMethod]
        paste("You selected a ", postHocTestFullName, " test with ", correctionMethodFullName, " adjustment.")
      } else {
        "Not a valid test type."
      }
    } else {
      "Not enough groups for post-hoc tests."
    }
  })
  
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

# Run the application 