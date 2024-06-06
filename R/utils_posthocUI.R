
create_comparisonsHeading <- function(input, shapiro_data_reactive) {
  renderUI({
    req(input$sampleInput, input$columnInput, input$group_comparison)
    data <- shapiro_data_reactive() # Assuming this returns your dataset
    
    num_groups <- length(unique(data$cell))
    
    if (num_groups == 2) {
      if (input$group_comparison == "parametric") {
        h4(HTML("<b>Independent T-Test</b>"))
      } else if (input$group_comparison == "welch") {
        h4(HTML("<b>Welch T-Test</b>"))
      } else {
        h4(HTML("<b>Mann-Whitney U Test</b>"))
      }
    } else if (num_groups > 2) {
      if (input$group_comparison == "parametric") {
        h4(HTML("<b>One-Way ANOVA</b>"))
      } else if (input$group_comparison == "welch") {
        h4(HTML("<b>Welch's ANOVA</b>"))
      } else {
        h4(HTML("<b>Kruskal-Wallis Test</b>"))
      }
    } else {
      return(NULL) # Handle the case where there are not enough groups
    }
  })
}


create_postHocUI <- function(input, shapiro_data_reactive){
  # Dynamically generate UI for post hoc options based on the type of test and number of groups
  renderUI({
    # Ensure we have more than 2 groups for post hoc tests to make sense
    req(input$sampleInput, input$columnInput, input$group_comparison)
    data <- shapiro_data_reactive() 
    num_groups <- length(unique(data$cell))
    
    if (input$group_comparison == "parametric" && num_groups > 2) {
      # Parametric post hoc test options
      radioButtons("postHocTest", HTML("<b>Select a post hoc test for ANOVA (if <i>p</i> < 0.05):</b>"),
                   choices = list("Tukey's HSD" = "tukey",
                                  "Bonferroni Correction" = "bonferroni",
                                  "Holm Correction" = "holm",
                                  "Benjamini-Hochberg Correction" = "bh",
                                  "Scheffé's Test" = "scheffe"))
    } else if (input$group_comparison == "non_parametric" && num_groups > 2) {
      # Nonparametric post hoc test options
      radioButtons("postHocTest", HTML("<b>Select a post hoc test for Kruskal-Wallis (if <i>p</i> < 0.05):</b>"),
                   choices = list("Dunn's Test" = "dunn",
                                  "Conover-Iman Test" = "conover"))
    } else if (input$group_comparison == "welch" && num_groups >2){
      radioButtons("postHocTest", HTML("<b>Select a post hoc test for Welch's ANOVA (if <i>p</i> < 0.05):</b>"),
                   choices = list("Games-Howell" = "games_howell",
                                  "Dunnett's T3" = "dunnett_t3"))
    } else {
      return(NULL) 
    }
  })
}

create_correctionUI <- function(input, shapiro_data_reactive){
  # Dynamically generate UI for correction options based on the post hoc test selected
  renderUI({
    num_groups <- length(unique(shapiro_data_reactive()$cell))
    # Ensure we have more than 2 groups for post hoc tests to make sense
    req(input$sampleInput, input$columnInput, input$group_comparison, num_groups > 2)
    if (!is.null(input$postHocTest) && (input$postHocTest == "dunn" || input$postHocTest == "conover")) {
      radioButtons("correctionMethod", HTML("<b>Select a correction method for Dunn's test:</b>"),
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
}

create_postHocHeading <- function(input, shapiro_data_reactive){
  renderUI({
    req(input$sampleInput, input$columnInput, input$group_comparison, input$postHocTest)
    data <- shapiro_data_reactive()
    num_groups <- length(unique(data$cell))
    if (input$group_comparison == "parametric" && input$postHocTest == "tukey" && num_groups > 2) {
      h4(HTML("<b>Tukey's HSD Post-hoc</b>"))
    } else if (input$group_comparison == "parametric" && input$postHocTest == "bonferroni" && num_groups > 2) {
      h4(HTML("<b>Pairwise t-test with Bonferroni adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "parametric" && input$postHocTest == "holm" && num_groups > 2) {
      h4(HTML("<b>Pairwise t-test with Holm adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "parametric" && input$postHocTest == "bh" && num_groups > 2) {
      h4(HTML("<b>Pairwise t-test with Benjamini-Hochberg adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "parametric" && input$postHocTest == "scheffe" && num_groups > 2) {
      h4(HTML("<b>Scheffé's Post-hoc</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "dunn" && input$correctionMethod == "bonferroni" && num_groups > 2) {
      h4(HTML("<b>Dunn's test with Bonferroni adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "dunn" && input$correctionMethod == "sidak" && num_groups > 2) {
      h4(HTML("<b>Dunn's test with Šidák adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "dunn" && input$correctionMethod == "holm" && num_groups > 2) {
      h4(HTML("<b>Dunn's test with Holm adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "dunn" && input$correctionMethod == "hs" && num_groups > 2) {
      h4(HTML("<b>Dunn's test with Holm-Šidák adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "dunn" && input$correctionMethod == "bh" && num_groups > 2) {
      h4(HTML("<b>Dunn's test with Benjamini-Hochberg adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "dunn" && input$correctionMethod == "hochberg" && num_groups > 2) {
      h4(HTML("<b>Dunn's test with Hochberg adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "conover" && input$correctionMethod == "bonferroni" && num_groups > 2) {
      h4(HTML("<b>Conover-Iman test with Bonferroni adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "conover" && input$correctionMethod == "sidak" && num_groups > 2) {
      h4(HTML("<b>Conover-Iman test with Šidák adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "conover" && input$correctionMethod == "holm" && num_groups > 2) {
      h4(HTML("<b>Conover-Iman test with Holm adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "conover" && input$correctionMethod == "hs" && num_groups > 2) {
      h4(HTML("<b>Conover-Iman test with Holm-Šidák adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "conover" && input$correctionMethod == "bh" && num_groups > 2) {
      h4(HTML("<b>Conover-Iman test with Benjamini-Hochberg adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "non_parametric" && input$postHocTest == "conover" && input$correctionMethod == "hochberg" && num_groups > 2) {
      h4(HTML("<b>Conover-Iman test with Hochberg adjustment for multiple comparisons</b>"))
    } else if (input$group_comparison == "welch" && input$postHocTest == "games_howell" && num_groups > 2) {
      h4(HTML("<b>Games-Howell post-hoc test</b>"))
    } else {
      return(NULL)
    }
  })
}

create_cldHeading <- function(comparisonResults){
  renderUI({
    req(!is.null(comparisonResults()$cld))
    tagList(
      tags$h4(HTML("<b>Compact Letter Display</b>")),
      tags$h6(HTML("Groups with the same letter are not significantly different from each other."))
    )
  })
}
