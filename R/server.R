

source("module_download.R")
source("utils_downloadGraphHandler.R")
source("utils_graphTheme.R")


server <- function(input, output, session) {
  
  # Reactive values to store housekeeper names and numeric input value
  housekeepers_names <- reactiveValues()
  
  # Reactive function to read and display the uploaded CSV file
  data <- reactive({
    req(input$file)
    
    # Read the CSV file
    df <- read.csv(input$file$datapath)
    
    return(df)
  })
  
  # Display the table using DataTable
  output$table <- renderDataTable({
    data()
  }, options = list(pageLength = 5))
  
  # Generate dynamic text input fields based on the number of groups
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
  
  # Generate the output text
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
    #move columns
    df <- data_relocate(df, select = "mean_hk", after = "Sample")
    df <- data_relocate(df, select = saved_variables$names, after = "Sample")
    
    #Calculate delta Cq for each target
    #which(names(data) == "mean_hk") finds the column index of "mean_hk" in the dataframe.
    #+ 1 increments the index to select the columns directly after "mean_hk".
    #ncol(data) provides the last column index of the dataframe.
    df <- df %>% 
      mutate(across((which(names(df) == "mean_hk") + 1):ncol(df), 
                    list(dct = ~ ifelse(.x != 0, .x - mean_hk, 0)), 
                    .names = "{.fn}_{.col}"))
    
    #calulcate fold change (relative mRNA)
    # Calculate fc, considering the case where the data point is 0
    df <- df %>% 
      mutate(across(
        (which(startsWith(names(df), "dct_"))):ncol(df),
        list(fc = ~ ifelse(.x != 0, 2^(-.x), 0)),
        .names = "{.fn}_{.col}"
      ))
    
    
    
    # Make a new column that places each sample as the specified condition
    df$condition <- gsub(".*_(\\w+)$", "\\1", df$Sample)
    df$condition <- as.factor(df$condition)
    #Add group data
    df$group <- gsub("^([^_]+)_.*$", "\\1", df$Sample)
    df$group <- as.factor(df$group)
    
    #add combined
    df$cell <- paste(df$condition, df$group, sep = "_")
    df$cell <- as.factor(df$cell)
    #Move column
    df <- data_relocate(df, select = "group", after = "Sample")
    df <- data_relocate(df, select = "condition", after = "Sample")
    df <- data_relocate(df, select = "cell", after = "group")
    
    return(df)
  })
  
  
  downloadServer("download_processed_data", wrangled_data, function(input, session) {
    paste("processed_PCR_data_", Sys.Date(), ".csv", sep = "")
  })
  
  
  
  # Display the table using DataTable in "Calculations" tab
  output$calculations_table <- renderDataTable({
    req(wrangled_data())
    wrangled_data()
  }, options = list(pageLength = 5, scrollX = TRUE, scrollY = "200px"))
  
  
  
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
  
  output$filtered_table <- renderDataTable({
    req(filtered_data())
    filtered_data()
  }, options = list(pageLength = 5, scrollX = TRUE, scrollY = "200px"))
  
  downloadServer("download_filtered_data", filtered_data, function(input, session) {
    condition <- input$condition  
    if (!is.null(condition)) {
      paste("filtered_PCR_data_", condition, "_", Sys.Date(), ".csv", sep = "")
    } else {
      paste("filtered_PCR_data_", Sys.Date(), ".csv", sep = "")
    }
  })
  
  # Calculate replicate averages when data is loaded
  rep_avg_data <- reactive({
    req(wrangled_data())
    
    vars <- colnames(wrangled_data()) %>%
      grep("^fc_dct", ., value = TRUE)
    
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
    condition <- input$condition  
    if (!is.null(condition)) {
      paste("Replicate_avg_data_", condition, "_", Sys.Date(), ".csv", sep = "")
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
    
    # Filter column names to include only those starting with "dct"
    dct_columns <- grep("^dct_", colnames(wrangled_data()), value = TRUE)
    
    # Generate selectInput for choosing the column dynamically
    selectInput("select_gene", "Select Gene to calculate DDCT", choices = dct_columns)
  })
  
  ddct_filtered_data <- reactive({
    req(wrangled_data())
    req(input$select_gene)
    
    condition2 <- input$select_condition
    control <- input$select_control
    samples <- input$select_samples
    selected_gene <- input$select_gene
    
    ddct_data <- wrangled_data() %>% 
      filter((group == control) | (group %in% samples)) %>% 
      filter(condition == condition2) %>%
      dplyr::select(group, condition, all_of(selected_gene))
    
    # Resetting levels of factors to only include selected options
    ddct_data$group <- factor(ddct_data$group, levels = unique(c(as.character(control), as.character(samples))))
    ddct_data$condition <- factor(ddct_data$condition, levels = condition2)
    return(ddct_data)
  })
  
  mean_value <- reactiveVal(NULL)
  # Calculate the average delta ct for the selected gene in the control samples
  average_dct <- reactive({
    req(wrangled_data())
    req(input$select_gene)
    req(input$select_control)
    
    condition3 <- input$select_condition
    selected_gene2 <- input$select_gene
    control2 <- input$select_control
    samples2 <- input$select_samples
    
    # Calculate the average delta ct for the selected gene in the control samples
    avg_dct_ctrl <- ddct_filtered_data() %>%
      filter(group == control2) %>%
      group_by(group, condition) %>%
      summarise(dct_ctrl_avg = mean(!!sym(selected_gene2), na.rm = TRUE), .groups = "drop")
    
    # Left join the original dataframe with the summarised dataframe
    avg_dct_ctrl <- left_join(ddct_filtered_data(), avg_dct_ctrl, by = c("group", "condition"))
    
    # Calculate the mean value
    mean_val <- mean(avg_dct_ctrl$dct_ctrl_avg, na.rm = TRUE)
    
    # Store the mean value in the reactive value
    mean_value(mean_val)
    
    # Assign the mean value to the entire dct_ctrl_avg column
    avg_dct_ctrl$dct_ctrl_avg <- mean_val
    
    # Create a new column ddct by subtracting selected_gene2 from dct_ctrl_avg
    avg_dct_ctrl$ddct <-  avg_dct_ctrl[[selected_gene2]] - avg_dct_ctrl$dct_ctrl_avg
    
    # Create a new column fc_ddct containing 2^(-ddct)
    avg_dct_ctrl$fc_ddct <- 2^(-avg_dct_ctrl$ddct)
    
    # Resetting levels of factors to only include selected options
    avg_dct_ctrl$group <- factor(avg_dct_ctrl$group, levels = unique(c(as.character(control2), as.character(samples2))))
    avg_dct_ctrl$condition <- factor(avg_dct_ctrl$condition, levels = condition3)
    avg_dct_ctrl$cell <- paste(avg_dct_ctrl$condition, avg_dct_ctrl$group, sep = "_")
    return(avg_dct_ctrl)
  })
  
  output$ddct_data <- renderDataTable({
    req(average_dct())
    average_dct()
  }, options = list(pageLength = 5))
  
  downloadServer("download_ddct_data", ddct_data, function(input, session) {
    paste("DDCT_processed_data_", Sys.Date(), ".csv", sep = "")
  })
  
  values <- reactiveValues(ddctDataSaved = FALSE)
  
  observeEvent(input$save_ddct_data, {
    # This block is executed whenever the 'Save ddct Data' button is clicked.
    # Even though you don't want to perform any action immediately when the button is clicked,
    # use this as a trigger for other reactive expressions or observers.
    values$ddctDataSaved <- TRUE
  })
  
  # Calculate replicate averages when data is loaded
  
  rep_avg_data_ddct <- reactive({
    req(average_dct())
    
    rep_avg_ddct <- average_dct() %>%
      group_by(condition, group) %>%
      summarize(mean_fc_ddct = mean(fc_ddct, na.rm = TRUE), .groups = "drop")
    
    #add column cell
    rep_avg_ddct$cell <- paste(rep_avg_ddct$condition, rep_avg_ddct$group, sep = "_")
    rep_avg_ddct$cell <- as.factor(rep_avg_ddct$cell)
    #Move column
    rep_avg_ddct <- data_relocate(rep_avg_ddct, select = "cell", after = "group")
    return(rep_avg_ddct)
  })
  
  # Display the replicate averages table in "Calculations" tab
  output$rep_avg_table_ddct <- renderDataTable({
    rep_avg_data_ddct()
  }, options = list(pageLength = 5))
  
  downloadServer("download_ddct_avg_data", rep_avg_table_ddct, function(input, session) {
    paste("DDCT_processed_replicate_data_", Sys.Date(), ".csv", sep = "")
  })
  
  #Stats
  observeEvent(input$select_dct_or_ddct_stats, {
    # Check if 'ddct' is selected
    if (input$select_dct_or_ddct_stats == "ddct_stats") {
      # Display the gene selection message
      output$selected_gene_ui_stats <- renderUI({
        textOutput("selected_gene_message_stats")
      })
    } else {
      # Hide the message when 'dct' is selected or for any other condition
      output$selected_gene_ui_stats <- renderUI({})
    }
  })
  
  # Define the text output for displaying the selected gene
  output$selected_gene_message_stats <- renderText({
    req(input$select_gene)  # Ensure there is a selection
    # Use gsub to remove "dct_" from the selected gene's name
    selected_gene_cleaned <- gsub("^dct_", "", input$select_gene)
    paste("You are currently performing stats on gene:", selected_gene_cleaned)
  })
  
  observe({
      if (input$select_dct_or_ddct_stats == "dct_stats") {
        updateSelectInput(session, "sampleInput", choices = unique(wrangled_data()$cell))
        updateSelectInput(session, "columnInput", choices = grep("^fc_dct_", names(wrangled_data()), value = TRUE))
      } else if (input$select_dct_or_ddct_stats == "ddct_stats") {
        # Check if avg_dct_df is not NULL and has the expected columns
        if (!is.null(values$ddctDataSaved) && values$ddctDataSaved) {
        updateSelectInput(session, "sampleInput", choices = unique(average_dct()$cell))
        updateSelectInput(session, "columnInput", choices = grep("^fc_ddct", names(average_dct()), value = TRUE))
        } else {
          updateSelectInput(session, "sampleInput", choices=character(0), selected = character(0))  # Clear the sampleInput choices
          updateSelectInput(session, "columnInput", choices=character(0), selected = character(0))
        }
      }
  })
  
  output$ddctMessage <- renderUI({
    # Check if 'ddct_stats' is selected and data is not saved yet
    if (input$select_dct_or_ddct_stats == "ddct_stats" && !values$ddctDataSaved) {
      # Return a UI element with the message
      tagList(
        HTML('<h5>Please go to the 2<sup>-(∆∆Ct)</sup> Calculations tab to create your ∆∆Ct dataset.</h5>'),
        tags$p("You need to save your ∆∆Ct dataset before proceeding.")
      )
    } else {
      # Return NULL or an empty UI element if conditions are not met
      return()
    }
  })
  
stats_data <- reactive({
  if (input$select_dct_or_ddct_stats == "dct_stats") {
    wrangled_data()
  } else if (input$select_dct_or_ddct_stats == "ddct_stats" && values$ddctDataSaved) {
    average_dct()
  } else{
    return(NULL)
  }
})
  
# Observe changes in the selection between dct and ddct
observeEvent(input$select_dct_or_ddct_stats, {
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
    # Calculate the count of non-NA entries for the selected fc_dct_ column for each sample
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
    req(input$select_dct_or_ddct_stats, input$sampleInput, input$columnInput, stats_data()) 
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
      p_value_summary <- ifelse(test_result$p.value > 0.05, "ns", ifelse(test_result$p.value < 0.01, "**", "*"))
      
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
  
  output$qqPlot <- renderPlot({
    req("qqplot" %in% input$normality_test, !is.null(input$columnInput))
    qqplot_data <- shapiro_data_reactive()
    # Generate the plot
    ggplot(qqplot_data, aes(sample = !!as.symbol(input$columnInput))) +
      geom_qq() + geom_qq_line() +
      facet_wrap(~cell, scales = "free_y") +
      labs(title = "QQ Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_Marnie
  })
  
  output$densityPlot <- renderPlot({
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
  
  output$levene <- renderDataTable({
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
      P_value_summary = ifelse(p_value > 0.05, "ns", ifelse(p_value < 0.01, "**", "*"))
    )
    summary_df <- summary_df %>% 
      rename("df (Group)" = DF_Group, "df (Error)" = DF_Error, "F" = F_Value, 
             "P Value" = P_Value, "Passed Variance Test?" = Passed_variance_test, "P Value Summary" = P_value_summary)
    rownames(summary_df) <- ""
    # Return the new summary data frame
    summary_df
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
        group_names <- unique(shapiro_data_reactive()$cell)
        group1_name <- group_names[1]
        group2_name <- group_names[2]
        # Perform t-test
        grp1 <-  shapiro_data_reactive() %>% filter(cell == unique(shapiro_data_reactive()$cell)[1]) %>% pull(!!as.symbol(input$columnInput))
        grp2 <-  shapiro_data_reactive() %>% filter(cell == unique(shapiro_data_reactive()$cell)[2]) %>% pull(!!as.symbol(input$columnInput))
        TTEST <- t.test(grp1, grp2, var.equal = TRUE)
        
        t_value <- abs(TTEST$statistic)
        df <- TTEST$parameter
        p_value <- TTEST$p.value
        significant <- ifelse(p_value < 0.05, "Yes", "No")
        p_value_summary <- ifelse(p_value > 0.05, "ns", ifelse(p_value < 0.01, "**", "*"))
        # Create the dataframe
        test_result_df <- data.frame(
          group1 = group1_name,
          group2 = group2_name,
          t = t_value,
          df = df,
          P.value_Two.Tailed = p_value,
          Significant = significant,
          P_value_summary = p_value_summary
        )
        rownames(test_result_df) <- ""
        test_result_df <- test_result_df %>% 
          rename("P Value (Two-Tailed)" = P.value_Two.Tailed, "Significant?" = Significant, "P Value Summary" = P_value_summary)
        #compact letter display
        groups <- unique(c(test_result_df$group1, test_result_df$group2))
        p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                           dimnames = list(groups, groups))
        for(i in 1:nrow(test_result_df)) {
          p_matrix[test_result_df$group1[i], test_result_df$group2[i]] <- test_result_df$"P Value (Two-Tailed)"[i]
          p_matrix[test_result_df$group2[i], test_result_df$group1[i]] <- test_result_df$"P Value (Two-Tailed)"[i]
        }
        cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
        cld_df <- data.frame(
          Group = names(cl_display$Letters),
          Letters = unname(cl_display$Letters)
        )
        
      } else {
        group_names <- unique(shapiro_data_reactive()$cell)
        group1_name <- group_names[1]
        group2_name <- group_names[2]
        # Perform Mann-Whitney U test
        grp1 <-  shapiro_data_reactive() %>% filter(cell == unique(shapiro_data_reactive()$cell)[1]) %>% pull(!!as.symbol(input$columnInput))
        grp2 <-  shapiro_data_reactive() %>% filter(cell == unique(shapiro_data_reactive()$cell)[2]) %>% pull(!!as.symbol(input$columnInput))
        mwu <- wilcox.test(grp1, grp2)
        
        w_value <- mwu$statistic
        p_value <- mwu$p.value
        significant <- ifelse(p_value < 0.05, "Yes", "No")
        p_value_summary <- ifelse(p_value > 0.05, "ns", ifelse(p_value < 0.01, "**", "*"))
        
        test_result_df <- data.frame(
          group1 = group1_name,
          group2 = group2_name,
          W = w_value,
          P.value = p_value,
          Significant = significant,
          P_value_summary = p_value_summary
        )
        #remove rowname of test_result_df
        rownames(test_result_df) <- ""
        test_result_df <- test_result_df %>% 
          rename("P Value" = P.value, "Significant?" = Significant, "P Value Summary" = P_value_summary)
        #compact letter display
        groups <- unique(c(test_result_df$group1, test_result_df$group2))
        p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                           dimnames = list(groups, groups))
        for(i in 1:nrow(test_result_df)) {
          p_matrix[test_result_df$group1[i], test_result_df$group2[i]] <- test_result_df$"P Value"[i]
          p_matrix[test_result_df$group2[i], test_result_df$group1[i]] <- test_result_df$"P Value"[i]
        }
        cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
        cld_df <- data.frame(
          Group = names(cl_display$Letters),
          Letters = unname(cl_display$Letters)
        )
      }
      return(list(test = test_result_df, cld = cld_df))
    } else if (num_groups > 2) {
      if (input$group_comparison == "parametric") {
        # Perform one-way ANOVA
        # Construct the formula as a string
        formula_str <- paste(input$columnInput, "~ cell")
        # Convert the string to a formula object
        aov_formula <- as.formula(formula_str)
        # Perform the ANOVA
        aov_result <- aov(aov_formula, data = shapiro_data_reactive())
        summary_aov <- summary(aov_result) # Storing the summary for potential display
        test_result_df <- as.data.frame(summary_aov[[1]])
        test_result_df$Significant <- ifelse(test_result_df$`Pr(>F)` < 0.05, "Yes", "No")
        test_result_df$P_value_summary <- ifelse(test_result_df$`Pr(>F)` > 0.05, "ns", ifelse(test_result_df$`Pr(>F)` < 0.01, "**", "*"))
        test_result_df <- test_result_df %>% 
          rename("P Value" = `Pr(>F)`, "Significant?" = Significant, "P Value Summary" = P_value_summary)
        
        post_hoc_df <- NULL
        if(input$postHocTest == "tukey"){
          post_hoc_result <- TukeyHSD(aov_result)
          post_hoc_df <- broom::tidy(post_hoc_result)
          post_hoc_df <- post_hoc_df %>%
            dplyr::select(-term, -null.value) %>%  # Remove term and null.value columns
            mutate(
              Significant = ifelse(adj.p.value < 0.05, "Yes", "No"),
              P_value_summary = ifelse(adj.p.value > 0.05, "ns", ifelse(adj.p.value < 0.01, "**", "*"))
            ) %>%
            rename("Adjusted P Value" = adj.p.value, 
                   "Significant?" = Significant, 
                   "P Value Summary" = P_value_summary,
                   Contrast = contrast,
                   Estimate = estimate,
                   "Confidence Low" = conf.low,
                   "Confidence High" = conf.high
            )
          split_names <- strsplit(post_hoc_df$Contrast, split = "-")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "Contrast")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          #remove Contrast column
          post_hoc_df <- post_hoc_df %>% dplyr::select(-Contrast)

          #compact letter display
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                             dimnames = list(groups, groups))
          for(i in 1:nrow(post_hoc_df)) {
            p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$`Adjusted P Value`[i]
            p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$`Adjusted P Value`[i]
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
          
        }else if (input$postHocTest == "bonferroni"){
          # Extract the response variable and the grouping factor based on the input formula
          response_var <- shapiro_data_reactive()[[input$columnInput]]
          group_factor <- shapiro_data_reactive()$cell
          # Perform the pairwise t-test with Bonferroni correction
          post_hoc_result <- pairwise.t.test(response_var, group_factor, p.adjust.method = "bonferroni")
          if(is.null(post_hoc_result$p.value) || all(post_hoc_result$p.value == "-", na.rm = TRUE)) {
            post_hoc_df <- data.frame(
              Message = "No p-values were computed. Check that ANOVA displayed p < 0.05"
            )
            cld_df <- data.frame(
              Message = "No p-values were computed"
            )
          } else {
          # Convert the result to a data frame for display. The `pairwise.t.test` function
          # returns a list with two components: p.value and method. The p.value component
          # is a matrix of the p-values of the tests. We'll convert this matrix to a tidy format.
          p_values_matrix <- post_hoc_result$p.value
          post_hoc_df <- as.data.frame(as.table(p_values_matrix))
          # Add a column to indicate whether the comparison is significant
          post_hoc_df$Significant <- ifelse(post_hoc_df$Freq < 0.05, "Yes", "No")
          # Add a summary of the p-value similar to what you've done before
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$Freq > 0.05, "ns", 
                                                ifelse(post_hoc_df$Freq < 0.01, "**", "*"))
          post_hoc_df <- post_hoc_df %>% 
            rename("Adjusted P Value" = Freq, "Significant?" = Significant, "P Value Summary" = P_value_summary, "group1" = Var1, "group2" = Var2)

          rownames(post_hoc_df) <- NULL
          #compact letter display
          #remove rows that have NA in Adjusted P Value
          post_hoc_df <- post_hoc_df[!is.na(post_hoc_df$`Adjusted P Value`), ]
          #change group1 group2 to as.character
          post_hoc_df$group1 <- as.character(post_hoc_df$group1)
          post_hoc_df$group2 <- as.character(post_hoc_df$group2)
          # Extract unique groups from both 'group1' and 'group2'
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          # Initialize the p_matrix with NA values and correct dimensions
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
          # Loop through each row in post_hoc_df to assign adjusted p-values
          for (i in 1:nrow(post_hoc_df)) {
            # Extract current groups and their adjusted p-value
            g1 <- post_hoc_df$group1[i]
            g2 <- post_hoc_df$group2[i]
            p_value <- post_hoc_df$`Adjusted P Value`[i]
            # Assign the adjusted p-value to the matrix, considering both [g1, g2] and [g2, g1]
            p_matrix[g1, g2] <- p_value
            p_matrix[g2, g1] <- p_value # Ensure symmetry
          }
          for(i in 1:nrow(post_hoc_df)) {
            if (post_hoc_df$group1[i] != post_hoc_df$group2[i]) {
              p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$`Adjusted P Value`[i]
              p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$`Adjusted P Value`[i]
            }
            # Else, it implicitly remains NA as initialized, correctly indicating no comparison
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
        }
          
        }else if (input$postHocTest == "holm"){
          # Extract the response variable and the grouping factor based on the input formula
          response_var <- shapiro_data_reactive()[[input$columnInput]]
          group_factor <- shapiro_data_reactive()$cell
          # Perform the pairwise t-test with Bonferroni correction
          post_hoc_result <- pairwise.t.test(response_var, group_factor, p.adjust.method = "holm")
          if(is.null(post_hoc_result$p.value) || all(post_hoc_result$p.value == "-", na.rm = TRUE)) {
            post_hoc_df <- data.frame(
              Message = "No p-values were computed. Check that ANOVA displayed p < 0.05"
            )
            cld_df <- data.frame(
              Message = "No p-values were computed"
            )
            }else{
          p_values_matrix <- post_hoc_result$p.value
          post_hoc_df <- as.data.frame(as.table(p_values_matrix))
          # Add a column to indicate whether the comparison is significant
          post_hoc_df$Significant <- ifelse(post_hoc_df$Freq < 0.05, "Yes", "No")
          # Add a summary of the p-value similar to what you've done before
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$Freq > 0.05, "ns", 
                                                ifelse(post_hoc_df$Freq < 0.01, "**", "*"))
          post_hoc_df <- post_hoc_df %>% 
            rename("Adjusted P Value" = Freq, "Significant?" = Significant, "P Value Summary" = P_value_summary, "group1" = Var1, "group2" = Var2)
          rownames(post_hoc_df) <- NULL
          #compact letter display
          #remove rows that have NA in Adjusted P Value
          post_hoc_df <- post_hoc_df[!is.na(post_hoc_df$`Adjusted P Value`), ]
          #change group1 group2 to as.character
          post_hoc_df$group1 <- as.character(post_hoc_df$group1)
          post_hoc_df$group2 <- as.character(post_hoc_df$group2)
          # Extract unique groups from both 'group1' and 'group2'
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          
          # Initialize the p_matrix with NA values and correct dimensions
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
          # Loop through each row in post_hoc_df to assign adjusted p-values
          for (i in 1:nrow(post_hoc_df)) {
            # Extract current groups and their adjusted p-value
            g1 <- post_hoc_df$group1[i]
            g2 <- post_hoc_df$group2[i]
            p_value <- post_hoc_df$`Adjusted P Value`[i]
            
            # Assign the adjusted p-value to the matrix, considering both [g1, g2] and [g2, g1]
            p_matrix[g1, g2] <- p_value
            p_matrix[g2, g1] <- p_value # Ensure symmetry
          }
          
          for(i in 1:nrow(post_hoc_df)) {
            if (post_hoc_df$group1[i] != post_hoc_df$group2[i]) {
              p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$`Adjusted P Value`[i]
              p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$`Adjusted P Value`[i]
            }
            # Else, it implicitly remains NA as initialized, correctly indicating no comparison
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
          }
        }else if (input$postHocTest == "bh"){
          df <- shapiro_data_reactive()
          # Extract the response variable and the grouping factor based on the input formula
          response_var <- df[[input$columnInput]]
          group_factor <- df$cell
          # Perform the pairwise t-test with Bonferroni correction
          post_hoc_result <- pairwise.t.test(response_var, group_factor, p.adjust.method = "BH")
          if(is.null(post_hoc_result$p.value) || all(post_hoc_result$p.value == "-", na.rm = TRUE)) {
            post_hoc_df <- data.frame(
              Message = "No p-values were computed. Check that ANOVA displayed p < 0.05"
            )
            cld_df <- data.frame(
              Message = "No p-values were computed"
            )
          }else{
          p_values_matrix <- post_hoc_result$p.value
          post_hoc_df <- as.data.frame(as.table(p_values_matrix))
          # Add a column to indicate whether the comparison is significant
          post_hoc_df$Significant <- ifelse(post_hoc_df$Freq < 0.05, "Yes", "No")
          # Add a summary of the p-value similar to what you've done before
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$Freq > 0.05, "ns", 
                                                ifelse(post_hoc_df$Freq < 0.01, "**", "*"))
          post_hoc_df <- post_hoc_df %>% 
            rename("Adjusted P Value" = Freq, "Significant?" = Significant, "P Value Summary" = P_value_summary, "group1" = Var1, "group2" = Var2)
          rownames(post_hoc_df) <- NULL
          #compact letter display
          #remove rows that have NA in Adjusted P Value
          post_hoc_df <- post_hoc_df[!is.na(post_hoc_df$`Adjusted P Value`), ]
          #change group1 group2 to as.character
          post_hoc_df$group1 <- as.character(post_hoc_df$group1)
          post_hoc_df$group2 <- as.character(post_hoc_df$group2)
          # Extract unique groups from both 'group1' and 'group2'
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          
          # Initialize the p_matrix with NA values and correct dimensions
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
          # Loop through each row in post_hoc_df to assign adjusted p-values
          for (i in 1:nrow(post_hoc_df)) {
            # Extract current groups and their adjusted p-value
            g1 <- post_hoc_df$group1[i]
            g2 <- post_hoc_df$group2[i]
            p_value <- post_hoc_df$`Adjusted P Value`[i]
            
            # Assign the adjusted p-value to the matrix, considering both [g1, g2] and [g2, g1]
            p_matrix[g1, g2] <- p_value
            p_matrix[g2, g1] <- p_value # Ensure symmetry
          }
          
          for(i in 1:nrow(post_hoc_df)) {
            if (post_hoc_df$group1[i] != post_hoc_df$group2[i]) {
              p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$`Adjusted P Value`[i]
              p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$`Adjusted P Value`[i]
            }
            # Else, it implicitly remains NA as initialized, correctly indicating no comparison
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
          }
        }else if (input$postHocTest == "scheffe"){
          # Construct the formula as a string
          formula_str <- paste(input$columnInput, "~ cell")
          # Convert the string to a formula object
          aov_formula <- as.formula(formula_str)
          # Perform the ANOVA
          aov_result <- aov(aov_formula, data = shapiro_data_reactive())
          post_hoc_result <- ScheffeTest(aov_result)
          post_hoc_df <- post_hoc_result$cell
          
          # Convert the matrix or list into a dataframe
          post_hoc_df <- as.data.frame(post_hoc_df)
          # Split the row names at the '-' character
          split_names <- strsplit(row.names(post_hoc_df), split = "-")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          # Rename columns appropriately if needed
          names(post_hoc_df) <- c("Difference", "Lower CI", "Upper CI", "P Value", "group1", "group2")
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "Difference")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          post_hoc_df$Significant <- ifelse(post_hoc_df$"P Value" < 0.05, "Yes", "No")
          # Add a summary of the p-value similar to what you've done before
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$"P Value" > 0.05, "ns", 
                                                ifelse(post_hoc_df$"P Value" < 0.01, "**", "*"))
          
          post_hoc_df <- post_hoc_df %>% 
            rename("Significant?" = Significant, "P Value Summary" = P_value_summary)
          rownames(post_hoc_df) <- NULL
          #compact letter display
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                             dimnames = list(groups, groups))
          for(i in 1:nrow(post_hoc_df)) {
            p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$"P Value"[i]
            p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$"P Value"[i]
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
        }
        
      } else {
        formula_str_kt <- paste(input$columnInput, "~ cell")
        kt_formula <- as.formula(formula_str_kt)
        KT <- kruskal.test(kt_formula, data = shapiro_data_reactive())
        H_value <- KT$statistic
        degf <- KT$parameter
        p_value <- KT$p.value
        
        # Determine significance and p-value summary
        significant <- ifelse(p_value < 0.05, "Yes", "No")
        p_value_summary <- ifelse(p_value > 0.05, "ns", ifelse(p_value < 0.01, "**", "*"))
        
        test_result_df <- data.frame(
          H = H_value,
          df = degf,
          P_value = p_value,
          Significant = significant,
          P_value_summary = p_value_summary
        )
        test_result_df <- test_result_df %>% 
          rename("P Value" = P_value, "Significant?" = Significant, "P Value Summary" = P_value_summary)
        
        post_hoc_df <- NULL
        if(input$postHocTest == "dunn" && input$correctionMethod == "bonferroni"){
          dependent_variable <- shapiro_data_reactive()[[input$columnInput]]
          group_variable <- shapiro_data_reactive()$cell
          
          # Perform the DunnTest with Bonferroni correction
          dunn_test_results <- FSA::dunnTest(dependent_variable ~ group_variable, 
                                             data = shapiro_data_reactive(), 
                                             method = "bonferroni")
          # Convert the test results to a dataframe for display
          post_hoc_df <- dunn_test_results$res
          split_names <- strsplit(post_hoc_df$Comparison, split = " - ")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          #remove comparison column
          post_hoc_df <- post_hoc_df %>% dplyr::select(-Comparison)
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "Z")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          rownames(post_hoc_df) <- NULL
          post_hoc_df$Significant <- ifelse(post_hoc_df$P.adj < 0.05, "Yes", "No")
          # Add a summary of the p-value 
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adj > 0.05, "ns", 
                                                ifelse(post_hoc_df$P.adj < 0.01, "**", "*"))
          
          post_hoc_df <- post_hoc_df %>% 
            rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
                   "Unadjusted P Value" = P.unadj, "Adjusted P Value" = P.adj)
          #compact letter display
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                             dimnames = list(groups, groups))
          for(i in 1:nrow(post_hoc_df)) {
            p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$"Adjusted P Value"[i]
            p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$"Adjusted P Value"[i]
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
        }else if(input$postHocTest == "dunn" && input$correctionMethod == "sidak"){
          dependent_variable <- shapiro_data_reactive()[[input$columnInput]]
          group_variable <- shapiro_data_reactive()$cell
          
          # Perform the DunnTest with Bonferroni correction
          dunn_test_results <- FSA::dunnTest(dependent_variable ~ group_variable, 
                                             data = shapiro_data_reactive(), 
                                             method = "sidak")
          # Convert the test results to a dataframe for display
          post_hoc_df <- dunn_test_results$res
          split_names <- strsplit(post_hoc_df$Comparison, split = " - ")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          #remove comparison column
          post_hoc_df <- post_hoc_df %>% dplyr::select(-Comparison)
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "Z")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          rownames(post_hoc_df) <- NULL
          post_hoc_df$Significant <- ifelse(post_hoc_df$P.adj < 0.05, "Yes", "No")
          # Add a summary of the p-value 
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adj > 0.05, "ns", 
                                                ifelse(post_hoc_df$P.adj < 0.01, "**", "*"))
          
          post_hoc_df <- post_hoc_df %>% 
            rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
                   "Unadjusted P Value" = P.unadj, "Adjusted P Value" = P.adj)
          #compact letter display
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                             dimnames = list(groups, groups))
          for(i in 1:nrow(post_hoc_df)) {
            p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$"Adjusted P Value"[i]
            p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$"Adjusted P Value"[i]
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
        }else if(input$postHocTest == "dunn" && input$correctionMethod == "hs"){
          dependent_variable <- shapiro_data_reactive()[[input$columnInput]]
          group_variable <- shapiro_data_reactive()$cell
          
          # Perform the DunnTest with Bonferroni correction
          dunn_test_results <- FSA::dunnTest(dependent_variable ~ group_variable, 
                                             data = shapiro_data_reactive(), 
                                             method = "hs")
          # Convert the test results to a dataframe for display
          post_hoc_df <- dunn_test_results$res
          split_names <- strsplit(post_hoc_df$Comparison, split = " - ")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          #remove comparison column
          post_hoc_df <- post_hoc_df %>% dplyr::select(-Comparison)
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "Z")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          rownames(post_hoc_df) <- NULL
          post_hoc_df$Significant <- ifelse(post_hoc_df$P.adj < 0.05, "Yes", "No")
          # Add a summary of the p-value 
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adj > 0.05, "ns", 
                                                ifelse(post_hoc_df$P.adj < 0.01, "**", "*"))
          
          post_hoc_df <- post_hoc_df %>% 
            rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
                   "Unadjusted P Value" = P.unadj, "Adjusted P Value" = P.adj)
          #compact letter display
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                             dimnames = list(groups, groups))
          for(i in 1:nrow(post_hoc_df)) {
            p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$"Adjusted P Value"[i]
            p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$"Adjusted P Value"[i]
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
        }else if(input$postHocTest == "dunn" && input$correctionMethod == "holm"){
          dependent_variable <- shapiro_data_reactive()[[input$columnInput]]
          group_variable <- shapiro_data_reactive()$cell
          
          # Perform the DunnTest with Bonferroni correction
          dunn_test_results <- FSA::dunnTest(dependent_variable ~ group_variable, 
                                             data = shapiro_data_reactive(), 
                                             method = "holm")
          # Convert the test results to a dataframe for display
          post_hoc_df <- dunn_test_results$res
          split_names <- strsplit(post_hoc_df$Comparison, split = " - ")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          #remove comparison column
          post_hoc_df <- post_hoc_df %>% dplyr::select(-Comparison)
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "Z")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          rownames(post_hoc_df) <- NULL
          post_hoc_df$Significant <- ifelse(post_hoc_df$P.adj < 0.05, "Yes", "No")
          # Add a summary of the p-value 
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adj > 0.05, "ns", 
                                                ifelse(post_hoc_df$P.adj < 0.01, "**", "*"))
          
          post_hoc_df <- post_hoc_df %>% 
            rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
                   "Unadjusted P Value" = P.unadj, "Adjusted P Value" = P.adj)
          #compact letter display
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                             dimnames = list(groups, groups))
          for(i in 1:nrow(post_hoc_df)) {
            p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$"Adjusted P Value"[i]
            p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$"Adjusted P Value"[i]
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
        }else if(input$postHocTest == "dunn" && input$correctionMethod == "bh"){
          dependent_variable <- shapiro_data_reactive()[[input$columnInput]]
          group_variable <- shapiro_data_reactive()$cell
          
          # Perform the DunnTest with Bonferroni correction
          dunn_test_results <- FSA::dunnTest(dependent_variable ~ group_variable, 
                                             data = shapiro_data_reactive(), 
                                             method = "bh")
          # Convert the test results to a dataframe for display
          post_hoc_df <- dunn_test_results$res
          split_names <- strsplit(post_hoc_df$Comparison, split = " - ")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          #remove comparison column
          post_hoc_df <- post_hoc_df %>% dplyr::select(-Comparison)
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "Z")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          rownames(post_hoc_df) <- NULL
          post_hoc_df$Significant <- ifelse(post_hoc_df$P.adj < 0.05, "Yes", "No")
          # Add a summary of the p-value 
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adj > 0.05, "ns", 
                                                ifelse(post_hoc_df$P.adj < 0.01, "**", "*"))
          
          post_hoc_df <- post_hoc_df %>% 
            rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
                   "Unadjusted P Value" = P.unadj, "Adjusted P Value" = P.adj)
          #compact letter display
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                             dimnames = list(groups, groups))
          for(i in 1:nrow(post_hoc_df)) {
            p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$"Adjusted P Value"[i]
            p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$"Adjusted P Value"[i]
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
        }else if(input$postHocTest == "dunn" && input$correctionMethod == "hochberg"){
          dependent_variable <- shapiro_data_reactive()[[input$columnInput]]
          group_variable <- shapiro_data_reactive()$cell
          
          # Perform the DunnTest with Bonferroni correction
          dunn_test_results <- FSA::dunnTest(dependent_variable ~ group_variable, 
                                             data = shapiro_data_reactive(), 
                                             method = "hochberg")
          # Convert the test results to a dataframe for display
          post_hoc_df <- dunn_test_results$res
          split_names <- strsplit(post_hoc_df$Comparison, split = " - ")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          #remove comparison column
          post_hoc_df <- post_hoc_df %>% dplyr::select(-Comparison)
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "Z")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          rownames(post_hoc_df) <- NULL
          post_hoc_df$Significant <- ifelse(post_hoc_df$P.adj < 0.05, "Yes", "No")
          # Add a summary of the p-value 
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adj > 0.05, "ns", 
                                                ifelse(post_hoc_df$P.adj < 0.01, "**", "*"))
          
          post_hoc_df <- post_hoc_df %>% 
            rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
                   "Unadjusted P Value" = P.unadj, "Adjusted P Value" = P.adj)
          #compact letter display
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                             dimnames = list(groups, groups))
          for(i in 1:nrow(post_hoc_df)) {
            p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$"Adjusted P Value"[i]
            p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$"Adjusted P Value"[i]
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
        }else if(input$postHocTest == "conover" && input$correctionMethod == "bonferroni"){
          dependent_variable <- shapiro_data_reactive()[[input$columnInput]]
          group_variable <- shapiro_data_reactive()$cell
          conover_result <- conover.test(dependent_variable, group_variable,
                                         method = "bonferroni")
          "T" <- conover_result$"T"
          P <- conover_result$P
          P.adjusted <- conover_result$P.adjusted
          comparisons <- conover_result$comparisons
          
          # Create a dataframe
          post_hoc_df <- data.frame(
            Comparison = comparisons,
            "T" = "T",
            P = P,
            P.adjusted = P.adjusted
          )
          split_names <- strsplit(post_hoc_df$Comparison, split = " - ")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          #remove comparison column
          post_hoc_df <- post_hoc_df %>% dplyr::select(-Comparison)
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "T")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          rownames(post_hoc_df) <- NULL
          post_hoc_df$Significant <- ifelse(post_hoc_df$P.adjusted < 0.05, "Yes", "No")
          # Add a summary of the p-value 
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adjusted > 0.05, "ns", 
                                                ifelse(post_hoc_df$P.adjusted < 0.01, "**", "*"))
          
          post_hoc_df <- post_hoc_df %>% 
            rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
                   "Unadjusted P Value" = P, "Adjusted P Value" = P.adjusted)
          #compact letter display
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                             dimnames = list(groups, groups))
          for(i in 1:nrow(post_hoc_df)) {
            p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$"Adjusted P Value"[i]
            p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$"Adjusted P Value"[i]
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
        }else if(input$postHocTest == "conover" && input$correctionMethod == "sidak"){
          dependent_variable <- shapiro_data_reactive()[[input$columnInput]]
          group_variable <- shapiro_data_reactive()$cell
          conover_result <- conover.test(dependent_variable, group_variable,
                                         method = "sidak")
          "T" <- conover_result$"T"
          P <- conover_result$P
          P.adjusted <- conover_result$P.adjusted
          comparisons <- conover_result$comparisons
          
          # Create a dataframe
          post_hoc_df <- data.frame(
            Comparison = comparisons,
            "T" = "T",
            P = P,
            P.adjusted = P.adjusted
          )
          split_names <- strsplit(post_hoc_df$Comparison, split = " - ")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          #remove comparison column
          post_hoc_df <- post_hoc_df %>% dplyr::select(-Comparison)
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "T")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          rownames(post_hoc_df) <- NULL
          post_hoc_df$Significant <- ifelse(post_hoc_df$P.adjusted < 0.05, "Yes", "No")
          # Add a summary of the p-value 
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adjusted > 0.05, "ns", 
                                                ifelse(post_hoc_df$P.adjusted < 0.01, "**", "*"))
          
          post_hoc_df <- post_hoc_df %>% 
            rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
                   "Unadjusted P Value" = P, "Adjusted P Value" = P.adjusted)
          #compact letter display
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                             dimnames = list(groups, groups))
          for(i in 1:nrow(post_hoc_df)) {
            p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$"Adjusted P Value"[i]
            p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$"Adjusted P Value"[i]
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
        }else if(input$postHocTest == "conover" && input$correctionMethod == "holm"){
          dependent_variable <- shapiro_data_reactive()[[input$columnInput]]
          group_variable <- shapiro_data_reactive()$cell
          conover_result <- conover.test(dependent_variable, group_variable,
                                         method = "holm")
          "T" <- conover_result$"T"
          P <- conover_result$P
          P.adjusted <- conover_result$P.adjusted
          comparisons <- conover_result$comparisons
          
          # Create a dataframe
          post_hoc_df <- data.frame(
            Comparison = comparisons,
            "T" = "T",
            P = P,
            P.adjusted = P.adjusted
          )
          split_names <- strsplit(post_hoc_df$Comparison, split = " - ")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          #remove comparison column
          post_hoc_df <- post_hoc_df %>% dplyr::select(-Comparison)
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "T")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          rownames(post_hoc_df) <- NULL
          post_hoc_df$Significant <- ifelse(post_hoc_df$P.adjusted < 0.05, "Yes", "No")
          # Add a summary of the p-value 
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adjusted > 0.05, "ns", 
                                                ifelse(post_hoc_df$P.adjusted < 0.01, "**", "*"))
          
          post_hoc_df <- post_hoc_df %>% 
            rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
                   "Unadjusted P Value" = P, "Adjusted P Value" = P.adjusted)  
          #compact letter display
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                             dimnames = list(groups, groups))
          for(i in 1:nrow(post_hoc_df)) {
            p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$"Adjusted P Value"[i]
            p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$"Adjusted P Value"[i]
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
        }else if(input$postHocTest == "conover" && input$correctionMethod == "bh"){
          dependent_variable <- shapiro_data_reactive()[[input$columnInput]]
          group_variable <- shapiro_data_reactive()$cell
          conover_result <- conover.test(dependent_variable, group_variable,
                                         method = "bh")
          "T" <- conover_result$"T"
          P <- conover_result$P
          P.adjusted <- conover_result$P.adjusted
          comparisons <- conover_result$comparisons
          
          # Create a dataframe
          post_hoc_df <- data.frame(
            Comparison = comparisons,
            "T" = "T",
            P = P,
            P.adjusted = P.adjusted
          )
          split_names <- strsplit(post_hoc_df$Comparison, split = " - ")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          #remove comparison column
          post_hoc_df <- post_hoc_df %>% dplyr::select(-Comparison)
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "T")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          rownames(post_hoc_df) <- NULL
          post_hoc_df$Significant <- ifelse(post_hoc_df$P.adjusted < 0.05, "Yes", "No")
          # Add a summary of the p-value 
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adjusted > 0.05, "ns", 
                                                ifelse(post_hoc_df$P.adjusted < 0.01, "**", "*"))
          
          post_hoc_df <- post_hoc_df %>% 
            rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
                   "Unadjusted P Value" = P, "Adjusted P Value" = P.adjusted)
          #compact letter display
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                             dimnames = list(groups, groups))
          for(i in 1:nrow(post_hoc_df)) {
            p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$"Adjusted P Value"[i]
            p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$"Adjusted P Value"[i]
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
        }else if(input$postHocTest == "conover" && input$correctionMethod == "hs"){
          dependent_variable <- shapiro_data_reactive()[[input$columnInput]]
          group_variable <- shapiro_data_reactive()$cell
          conover_result <- conover.test(dependent_variable, group_variable,
                                         method = "hs")
          "T" <- conover_result$"T"
          P <- conover_result$P
          P.adjusted <- conover_result$P.adjusted
          comparisons <- conover_result$comparisons
          
          # Create a dataframe
          post_hoc_df <- data.frame(
            Comparison = comparisons,
            "T" = "T",
            P = P,
            P.adjusted = P.adjusted
          )
          split_names <- strsplit(post_hoc_df$Comparison, split = " - ")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          #remove comparison column
          post_hoc_df <- post_hoc_df %>% dplyr::select(-Comparison)
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "T")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          rownames(post_hoc_df) <- NULL
          post_hoc_df$Significant <- ifelse(post_hoc_df$P.adjusted < 0.05, "Yes", "No")
          # Add a summary of the p-value 
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adjusted > 0.05, "ns", 
                                                ifelse(post_hoc_df$P.adjusted < 0.01, "**", "*"))
          
          post_hoc_df <- post_hoc_df %>% 
            rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
                   "Unadjusted P Value" = P, "Adjusted P Value" = P.adjusted)
          #compact letter display
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                             dimnames = list(groups, groups))
          for(i in 1:nrow(post_hoc_df)) {
            p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$"Adjusted P Value"[i]
            p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$"Adjusted P Value"[i]
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
        }else if(input$postHocTest == "conover" && input$correctionMethod == "hochberg"){
          dependent_variable <- shapiro_data_reactive()[[input$columnInput]]
          group_variable <- shapiro_data_reactive()$cell
          conover_result <- conover.test(dependent_variable, group_variable,
                                         method = "hochberg")
          "T" <- conover_result$"T"
          P <- conover_result$P
          P.adjusted <- conover_result$P.adjusted
          comparisons <- conover_result$comparisons
          
          # Create a dataframe
          post_hoc_df <- data.frame(
            Comparison = comparisons,
            "T" = "T",
            P = P,
            P.adjusted = P.adjusted
          )
          split_names <- strsplit(post_hoc_df$Comparison, split = " - ")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          #remove comparison column
          post_hoc_df <- post_hoc_df %>% dplyr::select(-Comparison)
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "T")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          rownames(post_hoc_df) <- NULL
          post_hoc_df$Significant <- ifelse(post_hoc_df$P.adjusted < 0.05, "Yes", "No")
          # Add a summary of the p-value 
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adjusted > 0.05, "ns", 
                                                ifelse(post_hoc_df$P.adjusted < 0.01, "**", "*"))
          
          post_hoc_df <- post_hoc_df %>% 
            rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
                   "Unadjusted P Value" = P, "Adjusted P Value" = P.adjusted)
          #compact letter display
          groups <- unique(c(post_hoc_df$group1, post_hoc_df$group2))
          p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                             dimnames = list(groups, groups))
          for(i in 1:nrow(post_hoc_df)) {
            p_matrix[post_hoc_df$group1[i], post_hoc_df$group2[i]] <- post_hoc_df$"Adjusted P Value"[i]
            p_matrix[post_hoc_df$group2[i], post_hoc_df$group1[i]] <- post_hoc_df$"Adjusted P Value"[i]
          }
          cl_display <- multcompLetters(p_matrix, compare = "<", threshold = 0.05)
          cld_df <- data.frame(
            Group = names(cl_display$Letters),
            Letters = unname(cl_display$Letters)
          )
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

  
  
  
  
  # Graphing dct or ddct  
  # Define a reactive expression to switch between datasets
  dct_or_ddct <- reactive({
    if (input$select_dct_or_ddct == "dct") {
      # If 'dct' is selected, return wrangled_data()
      return(wrangled_data())
    } else {
      # If 'ddct' is selected, return average_dct()
      return(average_dct())
    }
  })
  
  observeEvent(input$select_dct_or_ddct,{
    if (input$select_dct_or_ddct == "dct") {
      # Render the dynamic selectInput for choosing condition
      output$condition_selector <- renderUI({
        req(wrangled_data())  # Ensure data is available

        # Generate selectInput for choosing the condition dynamically
        selectInput("selected_condition", "Select Condition", choices = unique(wrangled_data()$cell),
                    multiple = TRUE)
      })
      
      # Render the dynamic selectInput for choosing the column
      output$column_selector <- renderUI({
        req(wrangled_data())  # Ensure data is available
        
        # Filter column names to include only those starting with "fc_dct"
        fc_dct_columns <- grep("^fc_dct", colnames(wrangled_data()), value = TRUE)
        
        # Generate selectInput for choosing the column dynamically
        selectInput("fc_dct_column", "Select Gene", choices = fc_dct_columns)
        
      })
    } else {
      # Hide the UI elements if ddct is selected
      output$condition_selector <- renderUI(NULL)
      output$column_selector <- renderUI(NULL)
    }
  })
  
  
  
  observeEvent(input$select_dct_or_ddct, {
    # Check if 'ddct' is selected
    if (input$select_dct_or_ddct == "ddct") {
      # Display the gene selection message
      output$selected_gene_ui <- renderUI({
        textOutput("selected_gene_message")
      })
    } else {
      # Hide the message when 'dct' is selected or for any other condition
      output$selected_gene_ui <- renderUI({})
    }
  })
  
  # Define the text output for displaying the selected gene
  output$selected_gene_message <- renderText({
    req(input$select_gene)  # Ensure there is a selection
    # Use gsub to remove "dct_" from the selected gene's name
    selected_gene_cleaned <- gsub("^dct_", "", input$select_gene)
    paste("You are currently graphing gene:", selected_gene_cleaned)
  })
  
  output$dynamic_y_label_input <- renderUI({
    # Initialize variable to store cleaned gene name
    selected_gene_cleaned <- ""
    
    # Determine which input to use based on the condition selected
    if (input$select_dct_or_ddct == "dct") {
      # Ensure the fc_dct_column input is used for dct condition
      req(input$fc_dct_column)  # Ensure there is a selection for the dct condition
      selected_gene_cleaned <- gsub("^fc_dct_", "", input$fc_dct_column)  # Clean the gene name for dct
    } else if (input$select_dct_or_ddct == "ddct") {
      # Assume there's another input mechanism for ddct or use a default value
      req(input$select_gene)  # Placeholder, adjust as necessary for ddct
      selected_gene_cleaned <- gsub("^dct_", "", input$select_gene)  # Clean the gene name for ddct
    }
    
    # Determine the placeholder text based on the selection
    placeholder_text <- if (input$select_dct_or_ddct == "dct") {
      paste0("Relative *", selected_gene_cleaned, "* mRNA (2<sup>-ΔCq</sup>)")
    } else if (input$select_dct_or_ddct == "ddct") {
      paste0("Fold change *", selected_gene_cleaned, "* mRNA (2<sup>-ΔΔCq</sup>)")
    } else {
      "Enter Y-axis Label"  # Default text if neither is selected
    }
    
    # Generate the text input with the dynamic placeholder
    textInput("y_label", "Enter Y-axis Label. Markup is accepted.", value = placeholder_text)
  })
  
  
  # Get the color scheme based on user input
  color_schemes <- reactiveValues(
    colourblind1 = c("#00359c", "#648fff", "#785ef0", "#dc267f", "#fe6100", "#ffb000"),
    colourblind2 = c("#ffbd00", "#ff5400", "#ff0054", "#9e0059", "#390099"),
    colourblind3 = c("#70d6ff", "#ff70a6", "#ff9770", "#ffd670", "#e9ff70"),
    colourblind4 = c("gray20", "#ff0166ff", "#117f80ff", "#40007fff", "#785ef0","#66ccfeff" ),
    grays = c("gray10", "gray30", "gray50", "gray70", "gray80", "gray100"),
    grays2 = c("#f8f9fa", "#e9ecef", "#dee2e6", "#ced4da", "#adb5bd", "#6c757d", "#495057", "#343a40", "#212529"),
    grays3 = c("#2b2d42", "#8d99ae", "#edf2f4"),
    electraGray = c("#e00154", "#222337", "#e6e1dd", "#b4a8b4", "#ddd2cf"),
    bones = c("#edede9", "#d6ccc2", "#f5ebe0", "#e3d5ca", "#d5bdaf"),
    oranges = c("#ffc971", "#ffb627", "#ff9505", "#e2711d" ,"#cc5803"),
    oranges2 = c("#ff4800", "#ff5400", "#ff6000", "#ff6d00", "#ff7900", "#ff8500", "#ff9100", "#ff9e00", "#ffaa00", "#ffb600"),
    pinks = c("#ffe5ec", "#ffc2d1", "#ffb3c6", "#ff8fab", "#fb6f92"),
    pinks2 = c("#590d22", "#800f2f", "#a4133c", "#c9184a", "#ff4d6d", "#ff758f", "#ff8fa3", "#ffb3c1", "#ffccd5", "#fff0f3"),
    blues = c("#03045e", "#0077b6", "#00b4d8", "#90e0ef", "#caf0f8"),
    blues2 = c("#03045e", "#023e8a", "#0077b6", "#0096c7", "#00b4d8", "#48cae4", "#90e0ef", "#ade8f4", "#caf0f8"),
    greens = c("#dad7cd", "#a3b18a", "#588157", "#3a5a40", "#344e41"),
    greens2 = c("#d8f3dc", "#b7e4c7", "#95d5b2", "#74c69d", "#52b788", "#40916c", "#2d6a4f", "#1b4332", "#081c15"),
    greens3 = c("#073b3a", "#0b6e4f", "#08a045", "#6bbf59"),
    green2purple = c("#35e95f", "#35d475", "#35ac7a", "#347f83", "#2e518a", "#40288f", "#5702a1", "#6500a3", "#8127b9"),
    purples = c("#c8b1e4", "#9b72cf", "#532b88", "#2f184b", "#f4effa"),
    purples3 = c("#7b2cbf", "#9d4edd", "#e0aaff" ),
    purple2orange = c("#9d53ff", "#b29ef8", "#f8d9c6", "#ffb57d", "#fb9649"),
    blaze = c("#8ecae6", "#219ebc", "#023047", "#ffb703", "#fb8500"),
    blaze2 = c("#14213d", "#fca311", "#C49792", "#e5e5e5"),
    peace = c("#2e58a4ff", "#b69e71ff", "#e3ded4ff", "#71aec7ff","#4f5357ff"),
    peace2 = c("#797d62", "#9b9b7a", "#d9ae94", "#f1dca7", "#ffcb69","#d08c60", "#997b66") ,
    ireland = c("#ff9f1c", "#ffbf69", "#e5e5e5", "#cbf3f0", "#2ec4b6"),
    twotone1 = c("#023e8a", "#0077b6", "#ff7900","#ff9e00"),
    twotone2 = c("#90b5daff", "#91b5daff", "#e47076ff", "#e46f74ff"),
    twotone3 = c("#447db3ff", "#e7953fff"),
    pastels = c("#ddfff7", "#93e1d8", "#ffa69e"),
    pastels2 = c("#90f1ef", "#ffd6e0", "#ffef9f"),
    pastels3 = c("#bdb2ff", "#ffcad4", "#b0d0d3" ),
    pastels4 = c("#cdb4db", "#ffc8dd", "#ffafcc", "#bde0fe", "#a2d2ff"),
    pastels5 = c("#ccd5ae", "#e9edc9", "#fefae0", "#faedcd", "#d4a373"),
    pastels6 = c("#ffadad", "#ffd6a5", "#fdffb6", "#caffbf", "#9bf6ff", "#a0c4ff", "#bdb2ff", "#ffc6ff", "#fffffc"),
    pastels7 = c("#809bce", "#95b8d1","#b8e0d2", "#d6eadf", "#eac4d5"),
    vibrant = c("#ff0f7b", "#f89b29" ),
    vibrant2 = c("#10e0ff", "#0086eb", "#006ee9", "#ffcd00", "#ffef00"),
    vibrant3 = c("#ff00c1", "#9600ff", "#4900ff", "#00b8ff", "#00fff9"),
    custom = c("#7400b8", "#6930c3", "#5e60ce", "#5390d9", "#4ea8de", "#56cfe1","#64dfdf", "#72efdd", "#80ffdb", "#B7D7B9", "#D2C3A8", "#E0B9A0","#EDAF97","#C49792", "#AD91A3", "#9D91A3")
  )
  
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
  
  fc_dct_columns_reactive <- reactive({
    req(wrangled_data())  # Ensure data is available
    grep("^fc_dct", colnames(wrangled_data()), value = TRUE)
  })
  
  # Now, you can access `fc_dct_columns_reactive()` as a reactive value.
  
  
  output$plot <- renderPlot({
    req(input$select_dct_or_ddct, input$y_label, input$x_label, input$fc_dct_column)
    set.seed(input$seed_input)
    
    if(input$select_dct_or_ddct == "dct"){
      filtered_data2 <- dct_or_ddct() %>%
        filter(cell %in% input$selected_condition)
      filtered_rep_avg_data2 <- rep_avg_data() %>%
        filter(cell %in% input$selected_condition)
      
      # Determine the x aesthetic based on the number of selected conditions
      x_aes <- if (length(input$selected_condition) >= 2) {
        sym("cell")
      } else {
        sym("group")
      }

      # Specify the y_aes based on user input
      y_aes <- sym(input$fc_dct_column)
      y_aes_avg <- sym(input$fc_dct_column)
      
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
      
    }else if(input$select_dct_or_ddct == "ddct"){
      filtered_data2 <- dct_or_ddct()
      filtered_rep_avg_data2 <- rep_avg_data_ddct()
      # Determine the x aesthetic based on the number of selected conditions
      x_aes <- sym("cell")
      # Specify the y_aes based on user input
      y_aes <- sym("fc_ddct")
      y_aes_avg <- sym("mean_fc_ddct")
      
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
    #if(input$select_dct_or_ddct == "dct"){      
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
      split_col <- 
      comparisonResults()$posthoc
      plot <- plot + stat_pvalue_manual(comparisonResults()$posthoc, label = "P Value Summary", y.position = input$yPos, label.size = input$sigSize, bracket.size = input$bracketSize, 
                                        step.increase = input$stepIncrease, hide.ns = input$hideNS, tip.length = input$tipLength, na.rm = TRUE, inherit.aes= FALSE)
    }else if(input$add_significance == "cld"){
      plot <- plot + stat_pvalue_manual(cld_df, label = "FILLIN", y.position("Makeitautomatic?"), label.size = input$sigSize, bracket.size = input$bracketSize, 
                                        step.increase = input$stepIncrease, hide.ns = input$hideNS, tip.length = input$tipLength, na.rm = TRUE, inherit.aes= FALSE)
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
  
  output$sigUI <- renderUI({
    if(input$add_significance == "asterix" || input$add_significance == "cld"){
      tagList(
      fluidRow(
        column(12, numericInput("yPos", "Y position", value = 1))
      ),  
      fluidRow(
        column(6, numericInput("sigSize", "Label Size", min = 0, max = 20, value = 5)),
        column(6, numericInput("bracketSize", "Bracket Size", min = 0, max = 10, value = 0.8, step = 0.1))
      ),
      fluidRow(
        column(6, numericInput("stepIncrease", "Step Increase", min = 0, max = 20, value = 0.1, step = 0.1)),
        column(6, numericInput("tipLength", "Tip Length", min = 0, max = 20, value = 0.02, step = 0.01))
      ),
      fluidRow(
        column(12, checkboxInput("hideNS", "Hide ns", value = FALSE))
      ),
      )
    }
  })
  
  output$downloadGraph <- downloadHandler(
    filename = function() {
      paste("your_graph_filename", ".", input$file_format, sep = "")
    },
    content = function(file) {
      # Use the `ggsave` function to save the plot as an SVG file
      ggsave(file, plot = last_plot(), device = input$file_format, dpi = input$dpi, width = input$width, height = input$height)
    })
  
 
}

# Run the application 