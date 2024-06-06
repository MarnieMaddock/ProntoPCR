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
    rownames(post_hoc_df) <- NULL
    return(list(post_hoc_df = post_hoc_df, cld_df = cld_df))
  }
}
