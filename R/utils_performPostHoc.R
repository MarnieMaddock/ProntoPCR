performPostHoc <- function(data, p_adjust_method, input_column = input$columnInput){
      # Extract the response variable and the grouping factor based on the input formula
      response_var <- data[[input_column]]
      group_factor <- data$cell
      # Perform the pairwise t-test with Bonferroni correction
      post_hoc_result <- pairwise.t.test(response_var, group_factor, p.adjust.method = p_adjust_method)
      # Initialize the data frames to be returned
      post_hoc_df <- data.frame()
      cld_df <- data.frame()
      
      # Check if the p-value is not available
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
        post_hoc_df$P_value_summary <- ifelse(post_hoc_df$Freq < 0.001, "***", 
                                              ifelse(post_hoc_df$Freq < 0.01, "**", 
                                                     ifelse(post_hoc_df$Freq > 0.05, "ns", "*")))
        post_hoc_df <- post_hoc_df %>% 
          rename("Adjusted P Value" = Freq, "Significant?" = Significant, "P Value Summary" = P_value_summary, "group1" = Var1, "group2" = Var2)
        
        rownames(post_hoc_df) <- NULL
        post_hoc_df <- post_hoc_df[!is.na(post_hoc_df$"Adjusted P Value"), ]
        
      }
      return(list(post_hoc_df = post_hoc_df, cld_df = cld_df))
}
