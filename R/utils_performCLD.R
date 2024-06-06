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
    cl_display <- multcompLetters(p_matrix, compare = "<=", threshold = 0.05)
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
    cl_display <- multcompLetters(p_matrix, compare = "<=", threshold = 0.05)
    cld_df <- data.frame(
      Group = names(cl_display$Letters),
      Letters = unname(cl_display$Letters)
    )

    
    return(cld_df)
  }  
}
