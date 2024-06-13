# Load the required libraries
source("module_checkCSVfile.R")
source("utils_dataWrangle.R")
source("utils_deltadelta.R")
source("module_download.R")
#source("utils_downloadGraphHandler.R")

#Stats functions
source("utils_statsUI.R")
source("utils_sampleSize.R")
source("utils_normPlots.R")
source("utils_levenes.R")
source("utils_shapiro.R")
source("utils_performComparisonTests.R")
source("utils_performCLD.R")
source("utils_performTukeyPostHoc.R")
source("utils_performPostHoc.R")
source("utils_performDunnPostHoc.R")
source("utils_performConoverPostHoc.R")
source("utils_posthocUI.R")

#Graph functions
source("utils_graphTheme.R")
source("utils_graphing.R")
source("utils_getColourSchemes.R")
source("utils_RmarkdownTemps.R")

server <- function(input, output, session) {
  
  #insert csv file and check that it meets the required formatting
  data <- checkCSVfile("file")
  downloadExampleData("file", dataset_path = "www/exampledata.csv")
  # Render the data table
  output$table <- render_data_table(data)
  
  # Generate dynamic text input fields for housekeeper genes
  output$groups <- generate_groups_ui(input)
  
  # Save text inputs as variables when the button is clicked
  saved_variables <- reactiveValues()
  save_housekeeper_names(input, saved_variables)
  
  # Render text output based on saved variables
  output$text1 <- render_housekeeper_text(input, saved_variables)
  
  # Render instruction text
  output$text2 <- render_instruction_text()
  
  # Data wrangling for dcq calculations
  wrangled_data <- wrangle_data(input, data, saved_variables)
  
  # Render the calculations table
  output$calculations_table <- render_calculations_table(wrangled_data)
    
  #download processed dcq data as a csv.
  downloadServer("download_processed_data", wrangled_data, function(input, session) {
    paste("processed_PCR_data_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
  })
  
  # filter data by condition UI part
  output$condition_filter <- filter_by_conditionUI(wrangled_data, input)
  
  # create filtered dataset
  filtered_data <- filtered_dataset(wrangled_data, input)
  
  #display filtered data in the UI
  output$filtered_table <- filtered_table_displayUI(filtered_data)
  
  #save condition as a string to put in the download handler
  condition_for_download <- save_condition_for_download(input)
  
  # download filtered data
  downloadServer("download_filtered_data", filtered_data, function(input, session) {
    if (!is.null(condition_for_download())) {
      paste("filtered_PCR_data_", condition_for_download(), "_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
    } else {
      paste("filtered_PCR_data_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
    }
  })
  
  # Calculate biological replicates for wrangled data
  rep_avg_data <- perform_rep_average(wrangled_data)
  
  # display the bio replicate data table in the UI
  output$rep_avg_table <- biorep_displayUI(rep_avg_data)
  
  # download replicate data
  downloadServer("download_rep_avg_data", rep_avg_data, function(input, session) {
    paste("Replicate_avg_data_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
  })
  
  # create filtered rep avg dataset
  filtered_rep_avg_data <- filtered_rep_avg_dataset(rep_avg_data, input)
  
  # display rep data filtered output in the UI
  output$rep_avg_filtered_table <- filtered_rep_displayUI(filtered_rep_avg_data)
  
  #download replicate average filtered dataset
  downloadServer("download_rep_avg_filtered_data", filtered_rep_avg_data, function(input, session) {
    if (!is.null(condition_for_download())) {
      paste("Replicate_avg_data_", condition_for_download(), "_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
    } else {
      paste("Replicate_avg_data_filtered_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
    }
  })

  # DELTADELTA Data and calculations
  # UI components for select control, samples and gene based of user input data
  output$select_control <- select_controlUI(wrangled_data)
  output$select_samples <- select_samplesUI(wrangled_data)
  output$column_selector2 <- select_geneUI(wrangled_data)

  # create ddcq dataset
  ddcq_filtered_data <- filtered_samples_for_ddcq_data(wrangled_data, input)
  
  # Calculate the average delta cq for the selected gene in the control samples
  mean_value <- reactiveVal(NULL)
  average_dcq <- calc_ddcq_data(wrangled_data, ddcq_filtered_data, input, mean_value)
  #display ddcq dataset in the UI
  output$ddcq_data <- render_ddcqUI(average_dcq)
  
  #save the condition to use in string for saving csv file
  gene_for_download <- reactive({
    input$select_gene
  })
  
  downloadServer("download_ddcq_data", average_dcq, function(input, session) {
    paste("DDCQ_processed_data_", gene_for_download(), "_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
  })
  
  values <- reactiveValues(ddcqDataSaved = FALSE)
  
  observeEvent(input$save_ddcq_data, {
    # This block is executed whenever the 'Save ddcq Data' button is clicked.
    # Even though you don't want to perform any action immediately when the button is clicked,
    # use this as a trigger for other reactive expressions or observers.
    values$ddcqDataSaved <- TRUE
  })
  
  # # Calculate replicate averages when ddcq is calculated
  rep_avg_data_ddcq <- calc_rep_avg_ddcq(average_dcq)
  #render result as table in UI
  output$rep_avg_table_ddcq <- render_ddcq_avgUI(rep_avg_data_ddcq)
  
  #download button to save replicate values
  downloadServer("download_ddcq_avg_data", rep_avg_data_ddcq, function(input, session) {
    paste("DDCq_processed_replicate_data_", gene_for_download(), "_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".csv", sep = "")
  })
  
  #STATISTICS SECTION
  # Define the text output for displaying the selected gene if dcq or ddcq is selected
  output$selected_gene_message_stats <- displayGeneUI_message(input)
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
#display UI for stats section selecting samples and gene
  observe({
    display_statsSamplesUI(input, session, values, wrangled_data, average_dcq)
  })
  #error messgae if DDCQ calculations havent occurred yet
  output$ddcqMessage <- ddcq_not_calculated_msg(input, values)
  #chose to use dcq or ddcq dataframes
  stats_data <- choose_stats_df(input, values, wrangled_data, average_dcq)

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

# Use the sampleSize function to dynamically render the heading in the UI
output$sampleSizeHeading <- render_sample_size_heading(input)

# Use the sampleSize function to calculate sample counts
sampleCounts <- calculate_sample_counts(input, stats_data)

# Use the sampleSize function to create sample size table
sampleSizeTable <- create_sample_size_table(sampleCounts)

# Use the sampleSize function to render the sample size table
output$nTable <- render_sample_size_table(input, sampleSizeTable)

  # #shapiro-wilk
  # # Dynamically render the heading based on the checkbox
output$normalityHeading <- render_shapiroHeading(input)
  
  # reactive dataset for selected samples
shapiro_data_reactive <- filter_data_stats(input, stats_data)

  # Reactive expression for performing the Shapiro-Wilk test
test_results_shapiro <- perform_shapiro(input, shapiro_data_reactive)
shapiro_results(input, output, test_results_shapiro)
  
  # Reactive expression for QQ plot using utils_normPlots.R
  qqPlot_reactive <- generate_qqPlot_reactive(input, shapiro_data_reactive)
  
  # Render the QQ plot using utils_normPlots.R
  output$qqPlot <- render_qqPlot(qqPlot_reactive)
  
  # Reactive expression for density plot
  densityPlot_reactive <- generate_densityPlot_reactive(input, shapiro_data_reactive)
  
  # Render the density plot using utils_normPlots.R
  output$densityPlot <- render_densityPlot(densityPlot_reactive)
  
  # Render UI for QQ plot using utils_normPlots.R
  output$qqPlotUI <- render_qqPlotUI(input)
  
  # Render UI for density plot using utils_normPlots.R
  output$densityPlotUI <- render_densityPlotUI(input)
  
  # display levene's heading when checkbox is clicked in sidepanel
  output$leveneHeading <- render_leveneHeading(input)
  
  #calculate levenes results and create dataframe
  levene_reactive <- calc_levenes(input, shapiro_data_reactive)
  
  # display levene's test
  output$levene <- levene_table(levene_reactive)
  output$leveneUI <- render_leveneUI(input, levene)
 
  comparisonResults <- reactive({
    # Ensure necessary inputs are available
    req(input$sampleInput, input$columnInput, input$group_comparison)
    data <- shapiro_data_reactive()

    
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    num_groups <- as.numeric(length(unique(data$cell)))

    test_result <- data.frame()
    test <- data.frame()
    test_result_df <- data.frame()
    posthoc <- data.frame()
    post_hoc_df <- data.frame()
    cld_df <- data.frame()
    cld <- data.frame()
    
    if (num_groups == 2) {
      if (input$group_comparison == "parametric") {
        #perform t-test
        test_result <- performComparisonTests(test_type = "t-test", data = shapiro_data_reactive(), column_input = input$columnInput)
        test_result_df <- test_result$test_result_df
        #compact letter display
        cld_df <- performCLD(data = test_result_df, p_colname = "P Value (Two-Tailed)",  remove_NA = FALSE)
      } else if (input$group_comparison == "welch"){
        test_result <- performComparisonTests(test_type = "welch_t", data = shapiro_data_reactive(), column_input = input$columnInput)
        test_result_df <- test_result$test_result_df
        #compact letter display
        cld_df <- performCLD(data = test_result_df, p_colname = "P Value",  remove_NA = FALSE)
      } else if (input$group_comparison == "non_parametric"){
        test_result <- performComparisonTests(test_type = "wilcox", data = shapiro_data_reactive(), column_input = input$columnInput)
        test_result_df <- test_result$test_result_df
        #compact letter display
        cld_df <- performCLD(data = test_result_df, p_colname = "P Value",  remove_NA = FALSE)
      } else {
        #return(NULL)
        return(list(test = test_result_df, cld = cld_df))
      }
      return(list(test = test_result_df, cld = cld_df))
    } else if (num_groups > 2) {
      if (isTRUE(input$group_comparison == "parametric")) {
        test_result <- performComparisonTests(test_type = "ANOVA", data = shapiro_data_reactive(), column_input = input$columnInput)
        test_result_df <- test_result$test_result_df
        aov_result  <- test_result$aov_result
            if(input$postHocTest == "tukey"){
              results <- performTukeyPostHoc(aov_df = aov_result, p_adjust_method = "tukey")
              # You can then access each dataframe like this:
              post_hoc_df <- results$post_hoc_df
              
              #compact letter display
              #cld_df <- results$cld_df
              cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
              
            }else if (input$postHocTest == "bonferroni"){
              try({
                # Validate conditions
                results <- performPostHoc(data = shapiro_data_reactive(), p_adjust_method = "bonferroni", input_column = input$columnInput, sample_sizes = sampleSizeTable())
                # You can then access each dataframe like this:
                post_hoc_df <- results$post_hoc_df
                
                #compact letter display
                #cld_df <- results$cld_df
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
            }else{
              #return(NULL)
              return(list(test = test_result_df, cld = cld_df))
            }
      } else if (isTRUE(input$group_comparison == "welch")){
        test_result <- performComparisonTests(test_type = "welch_ANOVA", data = shapiro_data_reactive(), column_input = input$columnInput)
        test_result_df <- test_result$test_result_df
        post_hoc_df <- NULL
        if(input$postHocTest == "games_howell"){
          df <- as.data.frame(shapiro_data_reactive())
          # Check group sizes
          group_sizes <- table(df$cell)
          if (any(group_sizes < 3)) {
            # Not enough data points in one or more groups
            post_hoc_df <- data.frame(Message = "Unable to compute Games-Howell test. Ensure that each group has ≥ 3 observations.")
            cld_df <- NULL
          } else {
          # Construct the formula as a string
          formula_str <- paste(input$columnInput, "~ cell")
          # Convert the string to a formula object
          test_formula <- as.formula(formula_str)
          post_hoc_df <- df %>%
            rstatix::games_howell_test(formula = test_formula)
          post_hoc_df <- post_hoc_df %>%
            dplyr::select(-.y.) %>%
            dplyr::select(-p.adj.signif) %>% 
            dplyr::rename(Estimate = estimate,
                          `Confidence Low` = conf.low,
                          `Confidence High` = conf.high,
                          `P Value (Tukey Adj)` = p.adj) %>%
            dplyr::mutate(Significance = ifelse(`P Value (Tukey Adj)` <= 0.001, "***",
                                                ifelse(`P Value (Tukey Adj)` <= 0.01, "**",
                                                       ifelse(`P Value (Tukey Adj)` > 0.05, "ns", "*"))))
          cld_df <- performCLD(data = post_hoc_df, p_colname = "P Value (Tukey Adj)",  remove_NA = FALSE)
          }
        }else{
          #return(NULL)
          return(list(test = test_result_df, cld = cld_df))
        }
      } else if (isTRUE(input$group_comparison == "non_parametric")){
        test_result <- performComparisonTests(test_type = "kw", data = shapiro_data_reactive(), column_input = input$columnInput)
        test_result_df <- test_result$test_result_df
        post_hoc_df <- NULL
        if(input$postHocTest == "dunn" && input$correctionMethod == "bonferroni"){
          try({
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
          }, silent = TRUE)
          
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
        }else{
          #return(NULL)
          return(list(test = test_result_df, cld = cld_df))
        }
        
      } else {
        #return(NULL) # Handle the case where there are not enough groups
        return(list(test = test_result_df, cld = cld_df))
      }
      
      return(list(test = test_result_df, posthoc = post_hoc_df, cld = cld_df))
    } else {
      return(NULL) # Handle the case where there are not enough groups
    }
  })
  

  
  # Render the dataframe using DT::renderDataTable
  output$dataTable <- renderDataTable({
    req(!is.null(comparisonResults()$test)) # Ensure the dataframe is ready
    if (!is.null(comparisonResults()$test)) {
      datatable(comparisonResults()$test, options = list(pageLength = 5, autoWidth = TRUE))
    } else {
      datatable(data.frame(Message = "No test results available"), options = list(pageLength = 5, autoWidth = TRUE))
    }
  })
  
  # Use renderUI to dynamically generate dataTableOutput
  output$testResultTable <- renderUI({
    # Dynamically create a dataTableOutput element
    dataTableOutput("dataTable")
  })
  
  
  output$postHocTable <- renderDataTable({
    req(!is.null(comparisonResults()$posthoc))  # Ensure the post-hoc results are available
    if (!is.null(comparisonResults()$posthoc)) {
      datatable(comparisonResults()$posthoc, options = list(pageLength = 5, autoWidth = TRUE))
    } else {
      return(NULL)
    }
  })
  
  output$postHocTableUI <- renderUI({
    # Dynamically create a dataTableOutput element
    dataTableOutput("postHocTable")
  })
  
  suppressWarnings({
    # Render the dataframe using DT::renderDataTable
    output$cld_table <- renderDataTable({
      req(!is.na(comparisonResults()$cld)) # Ensure the dataframe is ready
      if (!is.null(comparisonResults()$cld)) {
        datatable(comparisonResults()$cld, options = list(pageLength = 5, autoWidth = TRUE))
      } else {
        datatable(data.frame(Message = "No compact letter display results available"), options = list(pageLength = 5, autoWidth = TRUE))
      }
    })
  })

  
  
  # Use renderUI to dynamically generate dataTableOutput
  output$cld_tableUI <- renderUI({
    # Dynamically create a dataTableOutput element
    dataTableOutput("cld_table")
  })
  
  #Headings/UI generation for posthoc options
  output$comparisonsHeading <- create_comparisonsHeading(input, shapiro_data_reactive)
  output$postHocOptions <- create_postHocUI(input, shapiro_data_reactive)
  output$correctionOptions <- create_correctionUI(input, shapiro_data_reactive)
  output$postHocHeading <- create_postHocHeading(input, shapiro_data_reactive)
  output$cldHeading <- create_cldHeading(comparisonResults)
  
  #GRAPHING SECTION
  # Reactive value to track if the graph has been generated
  graph_generated <- reactiveVal(FALSE)
  
  output$ddcqMessage_graphs <- ddcq_not_calculated_msg_graphs(input, values)
  
  # Render the list of selected samples
  output$selected_samples_list <- display_ddcq_samples(input, rep_avg_data_ddcq)
  # # Define a reactive expression to switch between datasets
  # displays error msg if dcq and ddcq don't match, or if gene name doesn't match between graphs and stats tab
  dcq_or_ddcq <- select_dcq_ddcq_data(input, wrangled_data, average_dcq)
  observe({
    data_info  <- select_dcq_ddcq_data(input, wrangled_data, average_dcq)
    
  })
  #display UI components depending on if dcq or ddcq is selected
  dcq_ddcq_UI(input, output, wrangled_data)
  ddcq_UI(input, output, wrangled_data)
  
  # Define the text output for displaying the selected gene
  output$selected_gene_message <- displayGeneUI(input)
  
  #dynamically change Y axis according to selected gene
  output$dynamic_y_label_input <- dynamic_YLabel(input)
  
  # Get the color scheme based on user input
  color_schemes <- getColourSchemes()
  
  # Dynamic generation of text inputs based on positions
  output$x_axis_labels <- xAxis_label(input)
  
  # Reactive function to get user-entered labels
  user_labels <- userLabels_reactive(input)
  
  # Dynamic generation of text inputs based on positions
  xAxis_positions(input, output)
  
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
  
  ci <- function(x, confidence_level = 0.95) {
    n <- sum(!is.na(x))
    if (n > 1) {
      sd_x <- sd(x, na.rm = TRUE)
      mean_x <- mean(x, na.rm = TRUE)
      z <- qnorm((1 + confidence_level) / 2)  # Calculates the z-value for the specified confidence level
      error_margin <- z * sd_x / sqrt(n)
      return(data.frame(y = mean_x, ymin = mean_x - error_margin, ymax = mean_x + error_margin))
    } else {
      return(data.frame(y = NA, ymin = NA, ymax = NA))  # Return NA if not enough data
    }
  }
  
  # access `fc_dcq_columns_reactive()` as a reactive value.
  fc_dcq_columns_reactive <- gene_col_reactive(wrangled_data)
  
  # UI component for significance section in sidepanel of graphs tab
  num_groups <- calc_num_groups(shapiro_data_reactive)
  output$sigUI <- add_sigUI(input, num_groups)
  
  # render plot according to user input options
  output$plot <- create_graph(input, graph_generated, selected_stats = dcq_or_ddcq$selected_stats, selected_graphs = dcq_or_ddcq$selected_graphs, dcq_or_ddcq = dcq_or_ddcq$data, col_discrepancy = dcq_or_ddcq$col_discrepancy, rep_avg_data, rep_avg_data_ddcq, error_fun, color_schemes, colours, theme_Marnie, user_labels, shapiro_data_reactive, comparisonResults, ci)
  
  # Observe changes in data selection to reset the graph_generated flag
  observeEvent(input$select_dcq_or_ddcq, {
    graph_generated(FALSE)
  })
  
  observeEvent(input$select_dcq_or_ddcq_stats, {
    graph_generated(FALSE)
  })
  
  #If the user changes between dcq and ddcq different options appear 
  observeEvent(input$select_dcq_or_ddcq,{
    if (input$select_dcq_or_ddcq == "dcq") {
      # For DCQ: Render the dynamic selectInput for choosing conditions and columns
      output$condition_selector <- renderUI({
        req(wrangled_data())  # Ensure data is available
        
        selectInput("selected_condition", "Select Samples", choices = unique(wrangled_data()$cell),
                    multiple = TRUE)
      })
      
      output$column_selector <- renderUI({
        req(wrangled_data())  # Ensure data is available
        
        # Filter column names to include only those starting with "fc_dcq"
        fc_dcq_columns <- grep("^fc_dcq", colnames(wrangled_data()), value = TRUE)
        
        selectInput("fc_dcq_column", "Select Gene", choices = fc_dcq_columns)
      })
    } else {
      # For DDCQ: Hide the condition and column selectors
      output$condition_selector <- renderUI(NULL)
      output$column_selector <- renderUI(NULL)
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
      paste("Graph_", gene_name, "_", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".", input$file_format, sep = "")
    },
    content = function(file) {
      # Save the plot 
      ggsave(file, plot = last_plot(), device = input$file_format, dpi = input$dpi, width = input$width, height = input$height)
    }
  )
  
#STATISTICS REPORT
  selected_gene_name_stats <- clean_geneName(input)

  testName <- reactive({
    req(input$sampleInput, input$columnInput, input$group_comparison)
    data <- shapiro_data_reactive() # Assuming this returns your dataset
    
    num_groups <- length(unique(data$cell))
    
    if (num_groups == 2) {
      if (input$group_comparison == "parametric") {
        "Independent T-Test"
      } else if (input$group_comparison == "welch") {
        "Welch T-Test"
      } else {
        "Mann-Whitney U Test"
      }
    } else if (num_groups > 2) {
      if (input$group_comparison == "parametric") {
        "One-Way ANOVA"
      } else if (input$group_comparison == "non_parametric"){
        "Kruskal-Wallis Test"
      }else{
        "Welch's ANOVA"
      }
    } else {
      "Not enough groups" # Default message or handle as required
    }
  })
  # Define named vectors for the mappings
  fullPostHocTests <- c(tukey = "Tukey's HSD", 
                        dunn = "Dunn's", 
                        conover = "Conover-Iman",
                        games_howell = "Games-Howell"
                        # Add other mappings as necessary
  )
  
  fullCorrectionMethods <- c(bonferroni = "Bonferroni",
                             sidak = "Šidák",
                             holm = "Holm",
                             hs = "Holm-Šidák",
                             bh = "Benjamini-Hochberg",
                             hochberg = "Hochberg"
  )
  
  postHocTestDescription <-  create_postHocDescription(input, shapiro_data_reactive, fullPostHocTests, fullCorrectionMethods) 
  
  #download button UI for statistics results report
  output$downloadButtonUI <- downloadStatsButton(input)
  
  #download stats results as a report in HTML file
  create_downloadStatsReport(input, output, selected_gene_name_stats, sampleSizeTable, test_results_shapiro, 
                             qqPlot_reactive, densityPlot_reactive, levene_reactive, comparisonResults, testName, postHocTestDescription)

 #download graph options as html file
  create_downloadGraphOptions(input, output)
}

# Run the application 