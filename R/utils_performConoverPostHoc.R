performConoverPostHoc <- function(data, p_adjust_method, input_column = input$columnInput){
  post_hoc_df <- data.frame()
  cld_df <- data.frame()
  dependent_variable <- data[[input_column]]
  group_variable <- data$cell
  conover_result <- conover.test(dependent_variable, group_variable,
                                 method = p_adjust_method)
  "T" <- conover_result$T
  P <- conover_result$P
  P.adjusted <- conover_result$P.adjusted
  comparisons <- conover_result$comparisons
  
  # Create a dataframe
  post_hoc_df <- data.frame(
    Comparison = comparisons,
    "T" = T,
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
  post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adjusted < 0.001, "***",
                                        ifelse(post_hoc_df$P.adjusted < 0.01, "**",
                                               ifelse(post_hoc_df$P.adjusted > 0.05, "ns", "*")))
  
  post_hoc_df <- post_hoc_df %>% 
    rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
           "Unadjusted P Value" = P, "Adjusted P Value" = P.adjusted)
  return(list(post_hoc_df = post_hoc_df, cld_df = cld_df))
}