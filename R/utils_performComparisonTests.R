#function that performs the appropriate statistical test based on the user's selection
performComparisonTests <- function(test_type, data, column_input = columnInput()) {
  #initialize
  aov_result <- NULL
  residuals <- NULL
  test_result_df <- data.frame()
  #separate into groups
  group_names <- unique(data$cell)
  group1_name <- group_names[1]
  group2_name <- group_names[2]

  grp1 <-  data %>% dplyr::filter(cell == unique(data$cell)[1]) %>% dplyr::pull(!!column_input)
  grp2 <-  data %>% dplyr::filter(cell == unique(data$cell)[2]) %>% dplyr::pull(!!column_input)

  # Helper function to determine significance
  determine_significance <- function(p_value) {
    significant <- ifelse(p_value <= 0.05, "Yes", "No")
    p_value_summary <- ifelse(p_value <= 0.001, "***",
                              ifelse(p_value <= 0.01, "**",
                                     ifelse(p_value > 0.05, "ns", "*")))
    list(significant = significant, p_value_summary = p_value_summary)
  }
  # Perform the appropriate test based on the test_type
  switch(test_type,
         "t-test" = {
           TTEST <- stats::t.test(grp1, grp2, var.equal = TRUE)
           
           t_value <- abs(TTEST$statistic)
           df <- TTEST$parameter
           p_value <- TTEST$p.value
           significance_results <- determine_significance(p_value)
           
           # Create the dataframe
           test_result_df <- data.frame(
             group1 = group1_name,
             group2 = group2_name,
             t = t_value,
             df = df,
             P.value_Two.Tailed = p_value,
             Significant = significance_results$significant,
             P_value_summary = significance_results$p_value_summary
           )
           rownames(test_result_df) <- ""
           test_result_df <- test_result_df %>%
             dplyr::rename("P Value (Two-Tailed)" = P.value_Two.Tailed, "Significant?" = Significant, "P Value Summary" = P_value_summary)
         },
         "wilcox" = {
           mwu <- stats::wilcox.test(grp1, grp2)
           
           w_value <- mwu$statistic
           p_value <- mwu$p.value
           significance_results <- determine_significance(p_value)
           
           test_result_df <- data.frame(
             group1 = group1_name,
             group2 = group2_name,
             W = w_value,
             P.value = p_value,
             Significant = significance_results$significant,
             P_value_summary = significance_results$p_value_summary
           )
           rownames(test_result_df) <- ""
           test_result_df <- test_result_df %>%
             dplyr::rename("P Value" = P.value, "Significant?" = Significant, "P Value Summary" = P_value_summary)
         },
         "welch_t" = {
           TTEST <- stats::t.test(grp1, grp2, var.equal = FALSE)
           t_value <- abs(TTEST$statistic)
           df <- TTEST$parameter
           p_value <- TTEST$p.value
           significance_results <- determine_significance(p_value)
           
           # Create the dataframe
           test_result_df <- data.frame(
             group1 = group1_name,
             group2 = group2_name,
             t = t_value,
             df = df,
             P.value = p_value,
             Significant = significance_results$significant,
             P_value_summary = significance_results$p_value_summary
           )
           rownames(test_result_df) <- ""
           test_result_df <- test_result_df %>%
             dplyr::rename("P Value" = P.value, "Significant?" = Significant, "P Value Summary" = P_value_summary)
         },
         "ANOVA" = {
           # Perform one-way ANOVA
           formula_str <- paste(column_input, "~ cell")
           aov_formula <- as.formula(formula_str)
           aov_result <- stats::aov(aov_formula, data = data)
           summary_aov <- summary(aov_result)
           test_result_df <- as.data.frame(summary_aov[[1]])

           # Determine significance
           significance_results <- apply(test_result_df, 1, function(row) {
             determine_significance(row["Pr(>F)"])
           })
           # Extract significance and summary
           test_result_df$Significant <- sapply(significance_results, function(res) res$significant)
           test_result_df$P_value_summary <- sapply(significance_results, function(res) res$p_value_summary)
           
           test_result_df <- test_result_df %>%
             dplyr::rename("P Value" = `Pr(>F)`, "Significant?" = Significant, "P Value Summary" = P_value_summary)
           post_hoc_df <- data.frame()
         },
         "welch_ANOVA" = {
           # Perform Welch's one-way ANOVA
           formula_str <- paste(column_input, "~ cell")
           test_formula <- as.formula(formula_str)
           test_result <- stats::oneway.test(test_formula, data = data, var.equal = FALSE)
           F_value <- as.numeric(test_result$statistic)
           degf <- as.numeric(test_result$parameter["num df"])
           degf_error <- as.numeric(test_result$parameter["denom df"])
           p_value <- as.numeric(test_result$p.value)
           significance_results <- determine_significance(p_value)
           
           # Extract the results and create a dataframe
           test_result_df <- data.frame(
             `F value` = F_value,
             `Num df` = degf,
             `Denom df` = degf_error,
             `P Value` = p_value,
             `Significant?` = significance_results$significant,
             `P Value Summary` = significance_results$p_value_summary,
             check.names = FALSE
           )
           
           post_hoc_df <- data.frame()
         },
         "kw" = {
           formula_str_kt <- paste(column_input, "~ cell")
           kt_formula <- as.formula(formula_str_kt)
           KT <- stats::kruskal.test(kt_formula, data = data)
           H_value <- KT$statistic
           degf <- KT$parameter
           p_value <- KT$p.value
           # Determine significance and p-value summary
           significance_results <- determine_significance(p_value)
           
           test_result_df <- data.frame(
             H = H_value,
             df = degf,
             P.Value = p_value,
             Significant = significance_results$significant,
             P_value_summary = significance_results$p_value_summary
           )
           rownames(test_result_df) <- ""
           test_result_df <- test_result_df %>%
             dplyr::rename("P Value" = P.Value, "Significant?" = Significant, "P Value Summary" = P_value_summary)
         }
  )
  
  return(list(test_result_df = test_result_df, aov_result = aov_result))
}

  