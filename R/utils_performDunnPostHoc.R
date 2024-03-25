performDunnPostHoc <- function(data, p_adjust_method, input_column = input$columnInput){
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
    post_hoc_df <- data_relocate(post_hoc_df, select = "group1", before = "Z")
    post_hoc_df <- data_relocate(post_hoc_df, select = "group2", after = "group1")
    rownames(post_hoc_df) <- NULL
    post_hoc_df$Significant <- ifelse(is.na(post_hoc_df$P.adj), "NA",
                                      ifelse(post_hoc_df$P.adj < 0.05, "Yes", "No"))
    # Add a summary of the p-value
    post_hoc_df$P_value_summary <- ifelse(post_hoc_df$P.adj < 0.001, "***",
                                          ifelse(post_hoc_df$P.adj < 0.01, "**",
                                                 ifelse(post_hoc_df$P.adj > 0.05, "ns", "*")))

    post_hoc_df <- post_hoc_df %>%
      rename("Significant?" = Significant, "P Value Summary" = P_value_summary,
             "Unadjusted P Value" = P.unadj, "Adjusted P Value" = P.adj)
    return(list(post_hoc_df = post_hoc_df, cld_df = cld_df))
}
