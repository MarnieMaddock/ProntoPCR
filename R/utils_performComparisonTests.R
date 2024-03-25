performComparisonTests <- function(test_type, data, column_input = input$columnInput) {
  #initialize
  aov_result <- NULL
  #separate into groups
  group_names <- unique(data$cell)
  group1_name <- group_names[1]
  group2_name <- group_names[2]
  
  grp1 <-  data %>% filter(cell == unique(data$cell)[1]) %>% pull(!!column_input)
  grp2 <-  data %>% filter(cell == unique(data$cell)[2]) %>% pull(!!column_input)
  if(test_type == "t-test"){
    TTEST <- t.test(grp1, grp2, var.equal = TRUE)
    
    t_value <- abs(TTEST$statistic)
    df <- TTEST$parameter
    p_value <- TTEST$p.value
    significant <- ifelse(p_value < 0.05, "Yes", "No")
    p_value_summary <- ifelse(p_value < 0.001, "***", 
                              ifelse(p_value < 0.01, "**", 
                                     ifelse(p_value > 0.05, "ns", "*")))
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
  }else if (test_type == "wilcox"){
    mwu <- wilcox.test(grp1, grp2)
    
    w_value <- mwu$statistic
    p_value <- mwu$p.value
    significant <- ifelse(p_value < 0.05, "Yes", "No")
    p_value_summary <- ifelse(p_value < 0.001, "***", 
                              ifelse(p_value < 0.01, "**", 
                                     ifelse(p_value > 0.05, "ns", "*")))
    
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
  }else if (test_type == "ANOVA"){
    # Perform one-way ANOVA
    # Construct the formula as a string
    formula_str <- paste(column_input, "~ cell")
    # Convert the string to a formula object
    aov_formula <- as.formula(formula_str)
    # Perform the ANOVA
    aov_result <- aov(aov_formula, data = data)
    summary_aov <- summary(aov_result) # Storing the summary for potential display
    test_result_df <- as.data.frame(summary_aov[[1]])
    test_result_df$Significant <- ifelse(test_result_df$`Pr(>F)` < 0.05, "Yes", "No")
    test_result_df$P_value_summary <- ifelse(test_result_df$`Pr(>F)` < 0.001, "***",
                                             ifelse(test_result_df$`Pr(>F)` < 0.01, "**",
                                                    ifelse(test_result_df$`Pr(>F)` > 0.05, "ns", "*")))
    test_result_df <- test_result_df %>% 
      rename("P Value" = `Pr(>F)`, "Significant?" = Significant, "P Value Summary" = P_value_summary)
    post_hoc_df <- NULL
  }else if(test_type == "kw"){
    formula_str_kt <- paste(column_input, "~ cell")
    kt_formula <- as.formula(formula_str_kt)
    KT <- kruskal.test(kt_formula, data = data)
    H_value <- KT$statistic
    degf <- KT$parameter
    p_value <- KT$p.value
    
    # Determine significance and p-value summary
    significant <- ifelse(p_value < 0.05, "Yes", "No")
    p_value_summary <- ifelse(p_value < 0.001, "***", 
                              ifelse(p_value < 0.01, "**", 
                                     ifelse(p_value > 0.05, "ns", "*")))
    test_result_df <- data.frame(
      H = H_value,
      df = degf,
      P_value = p_value,
      Significant = significant,
      P_value_summary = p_value_summary
    )
    test_result_df <- test_result_df %>% 
      rename("P Value" = P_value, "Significant?" = Significant, "P Value Summary" = P_value_summary)
    

  }
  return(list(test_result_df = test_result_df, aov_result = aov_result))
}