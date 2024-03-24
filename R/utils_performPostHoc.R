performPostHoc <- function(data, p_adjust_method, input_column = input$columnInput, sample_sizes){
  # Initialize the data frames to be returned
  post_hoc_df <- data.frame()
  cld_df <- data.frame()

  # First, check if there are enough observations in each group
  if(any(sample_sizes$N <= 1, na.rm = TRUE)) {
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
      if(is.null(post_hoc_result$p.value) || all(post_hoc_result$p.value == "-", na.rm = TRUE) || (any(sample_sizes$N <= 1, na.rm = TRUE))) {
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
        # is a matrix of the p-values of the tests. We'll convert this matrix to a tidy format.
        p_values_matrix <- post_hoc_result$p.value
        post_hoc_df <- as.data.frame(as.table(p_values_matrix))
        # Add a column to indicate whether the comparison is significant
        post_hoc_df$Significant <- ifelse(post_hoc_df$Freq < 0.05, "Yes", "No")
        # Add a summary of the p-value similar to what you've done before
        post_hoc_df$P_value_summary <- ifelse(post_hoc_df$Freq < 0.001, "***", 
                                              ifelse(post_hoc_df$Freq < 0.01, "**", 
                                                     ifelse(post_hoc_df$Freq > 0.05, "ns", "*")))
        post_hoc_df <- post_hoc_df %>% 
          rename("Adjusted P Value" = Freq, "Significant?" = Significant, "P Value Summary" = P_value_summary, "group1" = Var1, "group2" = Var2)
        
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
          names(post_hoc_df) <- c("Difference", "Lower CI", "Upper CI", "Adjusted P Value", "group1", "group2")
          #move columns usinhg datawizard package
          post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "Difference")
          post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
          post_hoc_df$Significant <- ifelse(post_hoc_df$"Adjusted P Value" < 0.05, "Yes", "No")
          # Add a summary of the p-value similar to what you've done before
          post_hoc_df$P_value_summary <- ifelse(post_hoc_df$"Adjusted P Value" < 0.001, "***",
                                                ifelse(post_hoc_df$"Adjusted P Value" < 0.01, "**",
                                                       ifelse(post_hoc_df$"Adjusted P Value" > 0.05, "ns", "*")))
          
          post_hoc_df <- post_hoc_df %>% 
            rename("Significant?" = Significant, "P Value Summary" = P_value_summary)
          rownames(post_hoc_df) <- NULL
          #return(list(post_hoc_df = post_hoc_df, cld_df = cld_df))
  # } else if (post_hoc == "dunn" && p_adjust_method == "bonferroni" || p_adjust_method == "sidak" || p_adjust_method == "hs" || p_adjust_method == "holm" || p_adjust_method == "bh" || p_adjust_method == "hochberg"){
  #   dependent_variable <- data[[input_column]]
  #   group_variable <- data$cell
  #   
  #   # Perform the DunnTest with Bonferroni correction
  #   dunn_test_results <- FSA::dunnTest(dependent_variable ~ group_variable, 
  #                                      data = data, 
  #                                      method = p_adjust_method)
  #   print(dunn_test_results)
  #   # Convert the test results to a dataframe for display
  #   post_hoc_df <- dunn_test_results$res
  #   split_names <- strsplit(post_hoc_df$Comparison, split = " - ")
  #   
  #   # Create new columns for Group1 and Group2 based on the split row names
  #   post_hoc_df$group1 <- sapply(split_names, `[`, 1)
  #   post_hoc_df$group2 <- sapply(split_names, `[`, 2)
  #   #remove comparison column
  #   post_hoc_df <- post_hoc_df %>% dplyr::select(-Comparison)
  #   #move columns usinhg datawizard package
  #   post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "Z")
  #   post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
  #   rownames(post_hoc_df) <- NULL
  #   post_hoc_df$Significant <- ifelse(is.na(post_hoc_df$P.adj), "NA", 
  #                                     ifelse(post_hoc_df$P.adj < 0.05, "Yes", "No"))
  #   # Add a summary of the p-value 
  #   post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adj < 0.001, "***",
  #                                         ifelse(post_hoc_df$P.adj < 0.01, "**",
  #                                                ifelse(post_hoc_df$P.adj > 0.05, "ns", "*")))
  #   
  #   post_hoc_df <- post_hoc_df %>% 
  #     rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
  #            "Unadjusted P Value" = P.unadj, "Adjusted P Value" = P.adj)
  #   print(post_hoc_df)
  #   #return(list(post_hoc_df = post_hoc_df, cld_df = cld_df))
  #   }
  }
  }
  return(list(post_hoc_df = post_hoc_df, cld_df = cld_df))
}