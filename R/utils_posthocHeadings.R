# posthoc_functions.R
generatePostHocHeading <- function(input, data) {
  num_groups <- length(unique(data$cell))
  
  if (num_groups <= 2) {
    return(NULL)
  }
  
  if (input$group_comparison == "parametric") {
    if (input$postHocTest == "tukey") {
      h4(HTML("<b>Tukey's HSD Post-hoc</b>"))
    } else if (input$postHocTest == "bonferroni") {
      h4(HTML("<b>Pairwise t-test with Bonferroni adjustment for multiple comparisons</b>"))
    } else if (input$postHocTest == "holm") {
      h4(HTML("<b>Pairwise t-test with Holm adjustment for multiple comparisons</b>"))
    } else if (input$postHocTest == "bh") {
      h4(HTML("<b>Pairwise t-test with Benjamini-Hochberg adjustment for multiple comparisons</b>"))
    } else if (input$postHocTest == "scheffe") {
      h4(HTML("<b>Scheffé's Post-hoc</b>"))  
    }
  } else if (input$group_comparison == "non_parametric") {
    if (input$postHocTest == "dunn") {
      if (input$correctionMethod == "bonferroni") {
        h4(HTML("<b>Dunn's test with Bonferroni adjustment for multiple comparisons</b>"))
      } else if (input$correctionMethod == "sidak") {
        h4(HTML("<b>Dunn's test with Šidák adjustment for multiple comparisons</b>"))
      } else if (input$correctionMethod == "holm") {
        h4(HTML("<b>Dunn's test with Holm adjustment for multiple comparisons</b>"))
      } else if (input$correctionMethod == "hs") {
        h4(HTML("<b>Dunn's test with Holm-Šidák adjustment for multiple comparisons</b>"))
      } else if (input$correctionMethod == "bh") {
        h4(HTML("<b>Dunn's test with Benjamini-Hochberg adjustment for multiple comparisons</b>"))
      } else if (input$correctionMethod == "hochberg") {
        h4(HTML("<b>Dunn's test with Hochberg adjustment for multiple comparisons</b>"))
      }
    } else if (input$postHocTest == "conover") {
      if (input$correctionMethod == "bonferroni") {
        h4(HTML("<b>Conover-Iman test with Bonferroni adjustment for multiple comparisons</b>"))
      } else if (input$correctionMethod == "sidak") {
        h4(HTML("<b>Conover-Iman test with Šidák adjustment for multiple comparisons</b>"))
      } else if (input$correctionMethod == "holm") {
        h4(HTML("<b>Conover-Iman test with Holm adjustment for multiple comparisons</b>"))
      } else if (input$correctionMethod == "hs") {
        h4(HTML("<b>Conover-Iman test with Holm-Šidák adjustment for multiple comparisons</b>"))
      } else if (input$correctionMethod == "bh") {
        h4(HTML("<b>Conover-Iman test with Benjamini-Hochberg adjustment for multiple comparisons</b>"))
      } else if (input$correctionMethod == "hochberg") {
        h4(HTML("<b>Conover-Iman test with Hochberg adjustment for multiple comparisons</b>"))
      }
    }
  } else {
    return(NULL)
  }
}
