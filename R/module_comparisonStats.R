#module_comparisonStats.R

#side bar for comparison tests
compTestSidebar <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("group_comparison"), HTML("<b>5. Select the group comparisons to perform:</b>"), choices = c("Parametric Test" = "parametric", "Non-parametric Test" = "non_parametric", "Welch's Test" = "welch", "None" = "none"),
                 selected = "none"),
    uiOutput(ns("postHocOptions")), #display post hoc test options
    uiOutput(ns("correctionOptions")), #display post hoc adjustment methods
  )
}

#main panel for displaying stats output
compTestMain <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("comparisonsHeading")), # display the dynamic heading for comparisons
    uiOutput(ns("testResultTable")), # displays comparison test results
    uiOutput(ns("postHocHeading")), # displays dynamic post hoc heading depending on what is selected
    uiOutput(ns("postHocTableUI")), # displays post-hoc results
    uiOutput(ns("cldHeading")), # displays compact letter display heading if comparisons testing is selected
    uiOutput(ns("cld_tableUI")), # displays CLD results if selected.
    tags$br(),
    tags$br()
  )
}

#compute group comparisons based on selections made by the user
compTestServer <- function(id, sampleInput, columnInput, shapiro_data_reactive, sample_sizes, all_data, selected_stat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns 
    
    # save number of selected samples
    num_groups <- reactive({
      req(shapiro_data_reactive())
      length(unique(shapiro_data_reactive()$cell))
    })
    
    # save what group_comp was selected
    group_comparison <- reactive({
      req(input$group_comparison)
      input$group_comparison
    })
    
    
    # heading for comparisons testing for UI
    output$comparisonsHeading <- renderUI({
        req(sampleInput(), columnInput(), input$group_comparison)
      
        comparison_text <- switch(
          input$group_comparison,
          "parametric" = if (num_groups() == 2) "Independent T-Test" else "One-Way ANOVA",
          "welch" = if (num_groups() == 2) "Welch T-Test" else "Welch's ANOVA",
          "non_parametric" = if (num_groups() == 2) "Mann-Whitney U Test" else "Kruskal-Wallis Test"
        )
        
        if (!is.null(comparison_text)) {
          h4(HTML(paste0("<b>", comparison_text, "</b>")))
        } else {
          return(NULL) # Handle the case where there are not enough groups
        }
      })
    
    #post hoc
    output$postHocOptions <- renderUI({
      # Dynamically generate UI for post hoc options based on the type of test and number of groups
        # Ensure we have more than 2 groups for post hoc tests to make sense
        req(sampleInput(), columnInput(), input$group_comparison)
        if (input$group_comparison == "parametric" && num_groups() > 2) {
          # Parametric post hoc test options
          radioButtons(ns("postHocTest"), HTML("<b>Select a post hoc test for ANOVA (if <i>p</i> < 0.05):</b>"),
                       choices = list("Tukey's HSD" = "tukey",
                                      "Bonferroni Correction" = "bonferroni",
                                      "Holm Correction" = "holm",
                                      "Benjamini-Hochberg Correction" = "bh",
                                      "Scheffé's Test" = "scheffe"))
        } else if (input$group_comparison == "non_parametric" && num_groups() > 2) {
          # Nonparametric post hoc test options
          radioButtons(ns("postHocTest"), HTML("<b>Select a post hoc test for Kruskal-Wallis (if <i>p</i> < 0.05):</b>"),
                       choices = list("Dunn's Test" = "dunn"))
        } else if (input$group_comparison == "welch" && num_groups() >2){
          radioButtons(ns("postHocTest"), HTML("<b>Select a post hoc test for Welch's ANOVA (if <i>p</i> < 0.05):</b>"),
                       choices = list("Games-Howell" = "games_howell"))
        } else {
          return(NULL) 
        }
      })
    
    
    output$correctionOptions <- renderUI({
      # Dynamically generate UI for correction options based on the post hoc test selected
        # Ensure we have more than 2 groups for post hoc tests to make sense
        req(sampleInput(), columnInput(), input$group_comparison, num_groups() > 2)
        if (!is.null(input$postHocTest) && (input$postHocTest == "dunn" )) {
          radioButtons(ns("correctionMethod"), HTML("<b>Select a correction method for Dunn's test:</b>"),
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
    
    #post hoc headings
    output$postHocHeading <- renderUI({
      req(sampleInput(), columnInput(), input$group_comparison, input$postHocTest)
      if (num_groups() <= 2) return(NULL) # No post-hoc tests needed for <= 2 groups
      
      # Define a lookup table for post-hoc descriptions
      postHocDescriptions <- list(
        "parametric" = list(
          "tukey" = "Tukey's HSD Post-hoc",
          "bonferroni" = "Pairwise t-test with Bonferroni adjustment for multiple comparisons",
          "holm" = "Pairwise t-test with Holm adjustment for multiple comparisons",
          "bh" = "Pairwise t-test with Benjamini-Hochberg adjustment for multiple comparisons",
          "scheffe" = "Scheffé's Post-hoc"
        ),
        "non_parametric" = list(
          "dunn" = list(
            "bonferroni" = "Dunn's test with Bonferroni adjustment for multiple comparisons",
            "sidak" = "Dunn's test with Šidák adjustment for multiple comparisons",
            "holm" = "Dunn's test with Holm adjustment for multiple comparisons",
            "hs" = "Dunn's test with Holm-Šidák adjustment for multiple comparisons",
            "bh" = "Dunn's test with Benjamini-Hochberg adjustment for multiple comparisons",
            "hochberg" = "Dunn's test with Hochberg adjustment for multiple comparisons"
          )
        ),
        "welch" = list(
          "games_howell" = "Games-Howell post-hoc test"
        )
      )
      
      # Retrieve the description based on inputs
      postHocText <- NULL
      if (input$group_comparison %in% names(postHocDescriptions)) {
        if (input$postHocTest %in% names(postHocDescriptions[[input$group_comparison]])) {
          postHocEntry <- postHocDescriptions[[input$group_comparison]][[input$postHocTest]]
          
          # Check if there's a nested list for correction methods (non-parametric tests)
          if (is.list(postHocEntry)) {
            if (!is.null(input$correctionMethod) && input$correctionMethod %in% names(postHocEntry)) {
              postHocText <- postHocEntry[[input$correctionMethod]]
            } else {
              postHocText <- NULL # Handle cases where correction method is not applicable
            }
          } else {
            postHocText <- postHocEntry
          }
        }
      }
      
      if (!is.null(postHocText)) {
        h4(HTML(paste0("<b>", postHocText, "</b>")))
      } else {
        return(NULL)
      }
    })

    performPostHoc <- function(data, p_adjust_method, input_column = columnInput(), sample_sizes){
      # Initialize the data frames to be returned
      post_hoc_df <- data.frame()
      cld_df <- data.frame()
      
      # Check if there are enough observations in each group
      if(any(sample_sizes <= 1, na.rm = TRUE)) {
        test_result_df <- data.frame(
          Message = "Not enough observations in at least one of the groups."
        )
        # Not enough observations to perform post-hoc test
        post_hoc_df <- data.frame(
          Message = "Not enough observations in at least one of the groups."
        )
        cld_df <- data.frame(
          Message = "Not enough observations in the groups."
        )
        return(list(post_hoc_df = post_hoc_df, cld_df = cld_df, test_result_df = test_result_df))
      } else {
        
        if(p_adjust_method == "bonferroni" || p_adjust_method == "holm" || p_adjust_method == "BH"){
          # Extract the response variable and the grouping factor based on the input formula
          response_var <- data[[input_column]]
          group_factor <- data$cell
          # Perform the pairwise t-test with Bonferroni correction
          post_hoc_result <- pairwise.t.test(response_var, group_factor, p.adjust.method = p_adjust_method)
          if(is.null(post_hoc_result$p.value) || all(post_hoc_result$p.value == "-", na.rm = TRUE) || (any(sample_sizes <= 1, na.rm = TRUE))) {
            # Check if the p-value is not availablei
            post_hoc_df <- data.frame(
              Message = "No p-values were computed. Check that ANOVA displayed p < 0.05"
            )
            cld_df <- data.frame(
              Message = "No p-values were computed"
            )
            return(list(post_hoc_df = post_hoc_df, cld_df = cld_df))
          } else {
            # Convert the result to a data frame for display. The `pairwise.t.test` function
            # returns a list with two components: p.value and method. The p.value component
            # is a matrix of the p-values of the tests. Convert this matrix to a tidy format.
            p_values_matrix <- post_hoc_result$p.value
            post_hoc_df <- as.data.frame(as.table(p_values_matrix))
            # Add a column to indicate whether the comparison is significant
            post_hoc_df$Significant <- ifelse(post_hoc_df$Freq <= 0.05, "Yes", "No")
            # Add a summary of the p-value similar to what you've done before
            post_hoc_df$P_value_summary <- ifelse(post_hoc_df$Freq <= 0.001, "***", 
                                                  ifelse(post_hoc_df$Freq <= 0.01, "**", 
                                                         ifelse(post_hoc_df$Freq > 0.05, "ns", "*")))
            post_hoc_df <- post_hoc_df %>% 
              dplyr::rename("Adjusted P Value" = Freq, "Significant?" = Significant, "P Value Summary" = P_value_summary, "group1" = Var1, "group2" = Var2)
            
            rownames(post_hoc_df) <- NULL
            post_hoc_df <- post_hoc_df[!is.na(post_hoc_df$"Adjusted P Value"), ]
            return(list(post_hoc_df = post_hoc_df, cld_df = cld_df))
          }
          # return(list(post_hoc_df = post_hoc_df, cld_df = cld_df))
        }else if (p_adjust_method == "scheffe"){
          # Construct the formula as a string
          formula_str <- paste(input_column, "~ cell")
          # Convert the string to a formula object
          aov_formula <- as.formula(formula_str)
          # Perform the ANOVA
          aov_result <- aov(aov_formula, data = data)
          post_hoc_result <- DescTools::ScheffeTest(aov_result)
          post_hoc_df <- post_hoc_result$cell
          
          # Convert the matrix or list into a dataframe
          post_hoc_df <- as.data.frame(post_hoc_df)
          # Split the row names at the '-' character
          split_names <- strsplit(row.names(post_hoc_df), split = "-")
          
          # Create new columns for Group1 and Group2 based on the split row names
          post_hoc_df$group1 <- sapply(split_names, `[`, 1)
          post_hoc_df$group2 <- sapply(split_names, `[`, 2)
          # Rename columns appropriately if needed
          names(post_hoc_df) <- c("Difference", "Lower CI", "Upper CI", "Adjusted P Value", "group1", "group2")
          #move columns usinhg datawizard package
          post_hoc_df <- datawizard::data_relocate(post_hoc_df, select = "group1", before = "Difference")
          post_hoc_df <- datawizard::data_relocate(post_hoc_df, select = "group2", after = "group1")
          post_hoc_df$Significant <- ifelse(post_hoc_df$"Adjusted P Value" <= 0.05, "Yes", "No")
          # Add a summary of the p-value similar to what you've done before
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$"Adjusted P Value" <= 0.001, "***",
                                                ifelse(post_hoc_df$"Adjusted P Value" <= 0.01, "**",
                                                       ifelse(post_hoc_df$"Adjusted P Value" > 0.05, "ns", "*")))
          
          post_hoc_df <- post_hoc_df %>% 
            dplyr::rename("Significant?" = Significant, "P Value Summary" = P_value_summary)
          rownames(post_hoc_df) <- NULL
        }
      }
      return(list(post_hoc_df = post_hoc_df, cld_df = cld_df))
    }
    
    performTukeyPostHoc <- function(p_adjust_method, aov_df){
      # Initialize the data frames to be returned
      post_hoc_df <- data.frame()
      cld_df <- data.frame()
      if(p_adjust_method == "tukey"){
        post_hoc_result <- TukeyHSD(aov_df)
        post_hoc_df <- broom::tidy(post_hoc_result)
        post_hoc_df <- post_hoc_df %>%
          dplyr::select(-term, -null.value)  # Remove term and null.value columns
        post_hoc_df$Significant <- ifelse(post_hoc_df$adj.p.value <= 0.05, "Yes", "No")
        post_hoc_df$P_value_summary <- ifelse(post_hoc_df$adj.p.value <= 0.001, "***", 
                                              ifelse(post_hoc_df$adj.p.value <= 0.01, "**", 
                                                     ifelse(post_hoc_df$adj.p.value > 0.05, "ns", "*")))
        
        post_hoc_df <- post_hoc_df %>%
          dplyr::rename("Adjusted P Value" = adj.p.value, 
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
        post_hoc_df <- datawizard::data_relocate(post_hoc_df, select = "group1", before = "Contrast")
        post_hoc_df <- datawizard::data_relocate(post_hoc_df, select = "group2", after = "group1")
        #remove Contrast column
        post_hoc_df <- post_hoc_df %>% dplyr::select(-Contrast)
        rownames(post_hoc_df) <- NULL
        return(list(post_hoc_df = post_hoc_df, cld_df = cld_df))
      }
    }
    
    performDunnPostHoc <- function(data, p_adjust_method, input_column = columnInput()){
      post_hoc_df <- data.frame()
      cld_df <- data.frame()
      dependent_variable <- data[[input_column]]
      group_variable <- data$cell
      
      # Perform the DunnTest with Bonferroni correction
      dunn_test_results <- FSA::dunnTest(dependent_variable ~ group_variable,
                                         data = data,
                                         method = p_adjust_method)
      # Convert the test results to a dataframe for display
      post_hoc_df <- dunn_test_results$res
      split_names <- strsplit(post_hoc_df$Comparison, split = " - ")
      
      # Create new columns for Group1 and Group2 based on the split row names
      post_hoc_df$group1 <- sapply(split_names, `[`, 1)
      post_hoc_df$group2 <- sapply(split_names, `[`, 2)
      #remove comparison column
      post_hoc_df <- post_hoc_df %>% dplyr::select(-Comparison)
      #move columns usinhg datawizard package
      post_hoc_df <- datawizard::data_relocate(post_hoc_df, select = "group1", before = "Z")
      post_hoc_df <- datawizard::data_relocate(post_hoc_df, select = "group2", after = "group1")
      rownames(post_hoc_df) <- NULL
      post_hoc_df$Significant <- ifelse(is.na(post_hoc_df$P.adj), "NA",
                                        ifelse(post_hoc_df$P.adj <= 0.05, "Yes", "No"))
      # Add a summary of the p-value
      post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adj <= 0.001, "***",
                                            ifelse(post_hoc_df$P.adj <= 0.01, "**",
                                                   ifelse(post_hoc_df$P.adj > 0.05, "ns", "*")))
      
      post_hoc_df <- post_hoc_df %>%
        dplyr::rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
               "Unadjusted P Value" = P.unadj, "Adjusted P Value" = P.adj)
      return(list(post_hoc_df = post_hoc_df, cld_df = cld_df))
    }
    
    
    performCLD <- function(data, p_colname, remove_NA = FALSE){
      #compact letter display
      p_value_col <- data[[p_colname]]
      
      if (remove_NA){
        #change group1 group2 to as.character
        data$group1 <- as.character(data$group1)
        data$group2 <- as.character(data$group2)
        # Extract unique groups from both 'group1' and 'group2'
        groups <- unique(c(data$group1, data$group2))
        
        # Initialize the p_matrix with NA values and correct dimensions
        p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
        for (j in 1:nrow(data)) {
          if (is.na(data[[p_colname]][j])) {
            next  # Skip rows where the p-value is NA
          }
          g1 <- data$group1[j]
          g2 <- data$group2[j]
          p_value <- data[[p_colname]][j]
          # Proceed with existing logic...
        }
        
        for(i in 1:nrow(data)) {
          if (data$group1[i] != data$group2[i]) {
            p_matrix[data$group1[i], data$group2[i]] <- p_value_col[i]
            p_matrix[data$group2[i], data$group1[i]] <- p_value_col[i]
          }
          # Else, it implicitly remains NA as initialized, correctly indicating no comparison
        }
        
        # Generate compact letter display based on the p_matrix
        cl_display <- multcompView::multcompLetters(p_matrix, compare = "<=", threshold = 0.05)
        # Create dataframe for compact letter display results
        cld_df <- data.frame(
          Group = names(cl_display$Letters),
          Letters = unname(cl_display$Letters)
        )
        return(cld_df)
      } else { #for when NA does not need to be removed
        groups <- unique(c(data$group1, data$group2))
        p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), 
                           dimnames = list(groups, groups))
        for(i in 1:nrow(data)) {
          p_matrix[data$group1[i], data$group2[i]] <- p_value_col[i]
          p_matrix[data$group2[i], data$group1[i]] <- p_value_col[i]
        }
        cl_display <- multcompView::multcompLetters(p_matrix, compare = "<=", threshold = 0.05)
        cld_df <- data.frame(
          Group = names(cl_display$Letters),
          Letters = unname(cl_display$Letters)
        )
        
        
        return(cld_df)
      }  
    }
    
    


#reactive to perform stats test based on selections made by the user
    comparisonResults <- reactive({
      # Ensure necessary inputs are available
      req(sampleInput(), columnInput(), input$group_comparison)
      
      data <- shapiro_data_reactive()
      
      # Check if the column exists in the dataset
      if (!columnInput() %in% colnames(data)) {
        stop(paste("Column", columnInput(), "doesn't exist in the dataset..."))
      }
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      num_groups <- as.numeric(length(unique(data$cell)))
      
      # Check if "None" is selected

    if (input$group_comparison != "none") {
      # Check for insufficient groups and display appropriate error messages
      validate(
        need(num_groups >= 2, "Error: At least two groups are required for statistical tests.")
      )
      
      # Check for sufficient observations in the `y` variable
      # Extract sample sizes
      sample_sizes_vec <- sample_sizes()
      
      # Find groups with fewer than 2 observations
      insufficient_groups <- names(sample_sizes_vec)[sample_sizes_vec < 2]
      
      validate(
        need(length(insufficient_groups) == 0,
             paste(
               "Error: Insufficient observations for the selected gene to perform Comparison's Tests.")
             ))
    } else {
      return(NULL)  # Do nothing and display no output if "None" is selected
    }
      # Function to perform the test and CLD
      performTestAndCLD <- function(test_type, p_colname, remove_NA = FALSE) {
        test_result <- performComparisonTests(test_type = test_type, data = data, column_input = columnInput())
        cld_df <- performCLD(data = test_result$test_result_df, p_colname = p_colname, remove_NA = remove_NA)
        list(test = test_result$test_result_df, cld = cld_df)
      }
      
      # Function to perform post-hoc analysis and CLD
      performPostHocAndCLD <- function(method, p_colname = "Adjusted P Value", remove_NA = FALSE) {
        results <- performPostHoc(data = data, p_adjust_method = method, input_column = columnInput(), sample_sizes = sample_sizes())
        cld_df <- performCLD(data = results$post_hoc_df, p_colname = p_colname, remove_NA = remove_NA)
        list(posthoc = results$post_hoc_df, cld = cld_df)
      }
      
      # Initialize variables
      test_result_df <- NULL
      post_hoc_df <- NULL
      cld_df <- NULL
      
      # Process comparisons based on the number of groups
      if (num_groups == 2) {
        if (input$group_comparison == "parametric") {
          return(performTestAndCLD("t-test", "P Value (Two-Tailed)"))
        } else if (input$group_comparison == "welch") {
          return(performTestAndCLD("welch_t", "P Value"))
        } else if (input$group_comparison == "non_parametric") {
          return(performTestAndCLD("wilcox", "P Value"))
        }
      } else if (num_groups > 2) {
        req(input$postHocTest)
        if (isTRUE(input$group_comparison == "parametric")) {
          test_result <- performComparisonTests(test_type = "ANOVA", data = shapiro_data_reactive(), column_input = columnInput())
          test_result_df <- test_result$test_result_df
          aov_result  <- test_result$aov_result
          if(input$postHocTest == "tukey"){
            results <- performTukeyPostHoc(aov_df = aov_result, p_adjust_method = "tukey")
            # You can then access each dataframe like this:
            post_hoc_df <- results$post_hoc_df
            
            #compact letter display
            cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)

          }else if (input$postHocTest == "bonferroni"){
            try({
              # Validate conditions
              results <- performPostHoc(data = shapiro_data_reactive(), p_adjust_method = "bonferroni", input_column = columnInput(), sample_sizes = sample_sizes())
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
                input_column = columnInput(),
                sample_sizes = sample_sizes()
              )
              # Access the dataframe
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
                input_column = columnInput(),
                sample_sizes = sample_sizes()
              )
              # Access the dataframe
              post_hoc_df <- results$post_hoc_df

              #compact letter display
              cld_df <- results$cld_df
              cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = TRUE)
            }, silent = TRUE)
          }else if (input$postHocTest == "scheffe"){
            results <- performPostHoc(data = shapiro_data_reactive(), p_adjust_method = "scheffe", input_column = columnInput(), sample_sizes = sample_sizes())
            # Access the dataframe
            post_hoc_df <- results$post_hoc_df
            cld_df <- results$cld_df
            #compact letter display
            cld_df <- performCLD(data = post_hoc_df, p_colname = "Adjusted P Value",  remove_NA = FALSE)
          }else{
            #return(NULL)
            return(list(test = test_result_df, cld = cld_df))
          }
        } else if (isTRUE(input$group_comparison == "welch")){
          test_result <- performComparisonTests(test_type = "welch_ANOVA", data = shapiro_data_reactive(), column_input = columnInput())
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
              formula_str <- paste(columnInput(), "~ cell")
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
          req(input$correctionMethod)
          test_result <- performComparisonTests(test_type = "kw", data = shapiro_data_reactive(), column_input = columnInput())
          test_result_df <- test_result$test_result_df
          post_hoc_df <- NULL
          if(input$postHocTest == "dunn" && input$correctionMethod == "bonferroni"){
            try({
              # Validate conditions
              results <- performDunnPostHoc(
                data = shapiro_data_reactive(),
                p_adjust_method = "bonferroni",
                input_column = columnInput()
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
              input_column = columnInput()
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
              input_column = columnInput()
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
              input_column = columnInput()
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
              input_column = columnInput()
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
              input_column = columnInput()
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
    
    
    # Output or return the results from the module
    output$comparisonResults <- renderPrint({
      comparisonResults()
    })
    
    
    # Render the dataframe using DT::renderDataTable
    output$dataTable <- DT::renderDT({
      req(!is.null(comparisonResults()$test)) # Ensure the dataframe is ready
      if (!is.null(comparisonResults()$test)) {
        DT::datatable(comparisonResults()$test, options = list(pageLength = 5, autoWidth = TRUE))
      } else {
        DT::datatable(data.frame(Message = "No test results available"), options = list(pageLength = 5, autoWidth = TRUE))
      }
    })
    
    # Use renderUI to dynamically generate dataTableOutput
    output$testResultTable <- renderUI({
      # Dynamically create a dataTableOutput element
      DT::DTOutput(ns("dataTable"))
    })
    
    output$postHocTable <- DT::renderDT({
      req(!is.null(comparisonResults()$posthoc))  # Ensure the post-hoc results are available
      if (!is.null(comparisonResults()$posthoc)) {
        DT::datatable(comparisonResults()$posthoc, options = list(pageLength = 5, autoWidth = TRUE))
      } else {
        return(NULL)
      }
    })
    
    output$postHocTableUI <- renderUI({
      # Dynamically create a dataTableOutput element
      DT::DTOutput(ns("postHocTable"))
    })
    
    suppressWarnings({
      # Render the dataframe using DT::renderDataTable
      output$cld_table <- DT::renderDT({
        req(!is.na(comparisonResults()$cld)) # Ensure the dataframe is ready
        if (!is.null(comparisonResults()$cld)) {
          DT::datatable(comparisonResults()$cld, options = list(pageLength = 5, autoWidth = TRUE))
        } else {
          DT::datatable(data.frame(Message = "No compact letter display results available"), options = list(pageLength = 5, autoWidth = TRUE))
        }
      })
    })
    
    # Use renderUI to dynamically generate dataTableOutput
    output$cld_tableUI <- renderUI({
      # Dynamically create a dataTableOutput element
      DT::DTOutput(ns("cld_table"))
    })
    
    # Render the heading for the compact letter display
    output$cldHeading <- renderUI({
        req(!is.null(comparisonResults()$cld))
        tagList(
          tags$h4(HTML("<b>Compact Letter Display</b>")),
          tags$h6(HTML("Groups with the same letter are not significantly different from each other."))
        )
      })
    
    observeEvent(selected_stat(),{
      # Clear the selection in 'group_comparison' radio buttons
      updateRadioButtons(session, "group_comparison", selected = "none")
    })
    
    
    selected_gene_name_stats <- reactive({
      # Determine which input to use based on the condition selected
      if (input$select_dcq_or_ddcq_stats == "dcq_stats") {
        selected_gene_cleaned <- gsub("^fc_dcq_", "", input$columnInput)  # Clean the gene name for dcq
      } else if (input$select_dcq_or_ddcq_stats == "ddcq_stats") {
        selected_gene_cleaned <- gsub("^dcq_", "", input$select_gene)  # Clean the gene name for ddcq
      }
      return(selected_gene_cleaned)
    })
      
    #save the results in a reactiveValues object
    comparisonResults_rmd <- reactive({
      list(
        test = comparisonResults()$test,
        cld = comparisonResults()$cld,
        posthoc = comparisonResults()$posthoc
      )
    })
    
    posthoc_input <- reactive({
      req(input$postHocTest)
      input$postHocTest
    })
    
    correctionMethod_input <- reactive({
      req(input$correctionMethod)
      input$correctionMethod
    })
    
    return(list(comparisonResults = comparisonResults,
                group_comparison = group_comparison,
                posthoc_input = posthoc_input,
                correctionMethod_input = correctionMethod_input,
                comparisonResults_rmd = comparisonResults_rmd))
  })
}
