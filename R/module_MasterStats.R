#module_MasterStats.R
statsSidebar <- function(id) {
  ns <- NS(id)
  tagList(
    h5(HTML("<b>Data Selection</b>")),
    statsDataSidebar(ns("statsData")), # call statsDataSidebar from module_statsData.R
    tags$br(),
    h5(HTML("<b>Statistics</b>")),
    descriptiveSidebar(ns("descriptiveStats")), # call descriptiveSidebar from module_statsData.R
    normalitySidebar(ns("normalityStats")), # call normalitySidebar from module_normalityStats.R
    leveneSidebar(ns("leveneStats")), # call leveneSidebar from module_leveneStats.R
    logSidebar(ns("logStats")), # call logSidebar from module_logStats.R
    compTestSidebar(ns("compTest")), # call compTestSidebar from module_comparisonStats.R
    uiOutput(ns("downloadButtonUI")) #"Download Statistics Report"
  )
}

statsMain <- function(id) {
  ns <- NS(id)
  tagList(
    descriptiveMain(ns("descriptiveStats")), # call descriptiveMain from module_statsData.R
    normalityMain(ns("normalityStats")), # call normalityMain from module_normalityStats.R 
    leveneMain(ns("leveneStats")), # call leveneMain from module_leveneStats.R
    compTestMain(ns("compTest")) # call compTestMain from module_comparisonStats.R
  )
}

statsServer <- function(id, values, dcq_data, ddcq_data, ddcq_selected_gene, sampleInput, columnInput, stats_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    log <- logServer("logStats")
    

    #call statsDataServer from module_statsData.R
    wrangle_stats <- statsDataServer("statsData", values, dcq_data, ddcq_data, ddcq_selected_gene, log_transform = log)
    filter_data_stats <- wrangle_stats$filter_data_stats
    columnInput <- wrangle_stats$columnInput
    sampleInput <- wrangle_stats$sampleInput
    selected_stat <- wrangle_stats$selected_stat

    # descriptive stats
    descriptives <- descriptiveServer("descriptiveStats", sampleInput = sampleInput, columnInput = columnInput, stats_data = filter_data_stats)
    
    sample_sizes <- reactive({
      descriptives$computeSampleSizes()
    })
    
    descriptives_table <- reactive({
      descriptives$descriptivesTable()
    })

    # normality stats
    normalityModule <- normalityServer("normalityStats", sampleInput = sampleInput, columnInput = columnInput, stats_data = filter_data_stats, theme_Marnie = theme_Marnie, 
                    test_results = compTest, group_comparison = group_comparison, selected_stat = selected_stat)
    #levene test
    leveneModule <- leveneServer("leveneStats", columnInput = columnInput, stats_data = filter_data_stats, selected_stat = selected_stat)
    
    
    #comaprison tests
    compTest <- compTestServer("compTest", sampleInput = sampleInput, columnInput = columnInput, shapiro_data_reactive = filter_data_stats, sample_sizes = sample_sizes, all_data = dcq_data, selected_stat = selected_stat)
    
    comparisonResults <- reactive({
      compTest$comparisonResults()
    })
    
    group_comparison <- reactive({
      compTest$group_comparison()
    })
    
    posthoc_input <- reactive({
      compTest$posthoc_input()
    })
    
    
    correctionMethod_input <- reactive({
      compTest$correctionMethod_input()
    })
    
    selected_gene_name_stats <- reactive({
      # Determine which input to use based on the condition selected
      if (selected_stat() == "dcq_stats") {
        selected_gene_cleaned <- gsub("^fc_dcq_", "", columnInput())  # Clean the gene name for dcq
      } else if (selected_stat() == "ddcq_stats") {
        selected_gene_cleaned <- gsub("^dcq_", "", ddcq_selected_gene())  # Clean the gene name for ddcq
      } else if (selected_stat() == "housekeeper_stats") {
        selected_gene_cleaned <- "Housekeeper"  # Default housekeeper gene name
      } else {
        selected_gene_cleaned <- ""  # Default or error case
      }
      return(selected_gene_cleaned)
    })
    
    testName <- reactive({
      req(sampleInput(), columnInput(), group_comparison())
      num_groups <- length(unique(sampleInput()))
      req(group_comparison())
      
      if (num_groups == 2) {
        if (group_comparison() == "parametric") {
          "Independent T-Test"
        } else if (group_comparison() == "welch") {
          "Welch T-Test"
        } else {
          "Mann-Whitney U Test"
        }
      } else if (num_groups > 2) {
        if (group_comparison() == "parametric") {
          "One-Way ANOVA"
        } else if (group_comparison() == "non_parametric"){
          "Kruskal-Wallis Test"
        }else{
          "Welch's ANOVA"
        }
      } else {
        "Not enough groups" # Default message or handle as required
      }
    })

    # Define named vectors for the mappings
    fullPostHocTests <- c(tukey = "Tukey's HSD", 
                          dunn = "Dunn's", 
                          conover = "Conover-Iman",
                          games_howell = "Games-Howell"
                          # Add other mappings as necessary
    )
    
    fullCorrectionMethods <- c(bonferroni = "Bonferroni",
                               sidak = "Šidák",
                               holm = "Holm",
                               hs = "Holm-Šidák",
                               bh = "Benjamini-Hochberg",
                               hochberg = "Hochberg"
    )
    
    postHocTestDescription <- reactive({
      req(sampleInput(), group_comparison())
      num_groups <- length(unique(sampleInput()))
      
      # Logic to determine the test description based on the inputs
      if (num_groups > 2) { # Ensuring there are more than 2 groups
        
        if (group_comparison() == "parametric" || group_comparison() == "welch") {
          switch(posthoc_input(),
                 "tukey" = "You selected a Tukey's HSD Post-hoc test.",
                 "bonferroni" = "You selected a Pairwise t-test with Bonferroni adjustment.",
                 "holm" = "You selected a Pairwise t-test with Holm adjustment.",
                 "bh" = "You selected a Pairwise t-test with Benjamini-Hochberg adjustment.",
                 "scheffe" = "You selected a Scheffé's Post-hoc test.",
                 "games_howell" = "You selected a Games-Howell Post-hoc test with Tukey adjustment.",
                 "Not a valid parametric post-hoc test"
          )
        } else if (group_comparison() == "non_parametric") {
          postHocTestFullName <- fullPostHocTests[posthoc_input()]
          correctionMethodFullName <- fullCorrectionMethods[correctionMethod_input()]
          paste("You selected a", postHocTestFullName, "test with", correctionMethodFullName, "adjustment.")
        } else {
          "Not a valid test type."
        }
      } else if (num_groups == 2) {
        # Return a message indicating no post-hoc tests are performed for two groups
        "Post-hoc tests are not applicable when there are only two groups."
      } else {
        "Not enough groups for post-hoc tests."
      }
    })
    
    output$downloadButtonUI <- renderUI({
        downloadButton(ns("downloadStats"), "Download Statistics Report")
    })
    
    output$downloadStats <- downloadHandler(
      filename = function() {
        paste("StatisticsReport-", Sys.Date(), "-", format(Sys.time(), "%H-%M-%S"), ".html", sep="")
      },
      content = function(file) {
        # Specify the path to your R Markdown template
        rmdTemplate <- "inst/rmd_templates/StatisticsReport.Rmd"
        
        # Print dcq or ddcq
        analysisHTML <- if (selected_stat() == "dcq_stats") {
          "2<sup>-(ΔCq)</sup>"
        } else if (selected_stat() == "ddcq_stats") {
          "2<sup>-(ΔΔCq)</sup>"
        } else if (selected_stat() == "housekeeper_stats") {
          "Housekeeper Mean"
        } else {
          ""  # Default or error case
        }
        
        # Get the cleaned gene name
        cleanedGeneName <- isolate(selected_gene_name_stats())
        
        # Handle the descriptive statistics table
        descriptivesTable <- if (length(descriptives$descriptives_input()) > 0) {
          isolate(descriptives$descriptivesResults())
        } else {
          NULL
        }
        
        # Generate the shapiro-wilk table data
        shapiroTable <- if ("shapiro" %in% normalityModule$normality_input()) {
          isolate(normalityModule$normalityResults()$shapiro) # Accessing shapiro result from the module. # Ensure this is not reactive here, or use isolate() if needed
        } else {
          NULL
        }
        
        resPlot <- if ("res_fit_plot_rmd" %in% normalityModule$normality_input()) {
          isolate(normalityModule$normalityResults()$res_fit_plot_rmd)
        } else {
          NULL
        }
       
        # Generate the qq plot
        qqPlot <- if ("qqplot" %in% normalityModule$normality_input()) {
          isolate(normalityModule$normalityResults()$qqPlot)  # Accessing QQ plot from the module
        } else {
          NULL
        }
        
        # Generate the density plot
        densityPlot <- if ("density" %in% normalityModule$normality_input()) {
          isolate(normalityModule$normalityResults()$densityPlot)  # Accessing density plot from the module
        } else {
          NULL
        }
        
        leveneTable <- if (leveneModule$levene_input() == TRUE) {
          isolate(leveneModule$leveneResults())
        } else {
          NULL
        }
        
        logResult <- if (log() != "None") {
          TRUE
        } else {
          FALSE
        }
        
        
        comparisonResultsData <- if(group_comparison() == "parametric" || group_comparison() == "non_parametric" || group_comparison() == "welch"){
          isolate(comparisonResults())
        } else {
          NULL
        }
        
        testNames <- if(group_comparison() == "parametric" || group_comparison() == "non_parametric" || group_comparison() == "welch"){
          isolate(testName())
        } else {
          NULL
        }
        postHocNames <- if(group_comparison() == "parametric" || group_comparison() == "non_parametric" || group_comparison() == "welch"){
          isolate(postHocTestDescription())
        } else {
          NULL
        }
        # Render the Rmarkdown report
        rmarkdown::render(input = rmdTemplate, 
                          output_file = file,
                          params = list(
                            analysisType = analysisHTML,
                            selectedSamples = isolate(sampleInput()),
                            selectedGene = cleanedGeneName,
                            descTable = descriptivesTable,
                            shapiroData = shapiroTable,
                            resFitPlot = resPlot,
                            qqPlotGraph = qqPlot,
                            densityPlot = densityPlot,
                            levene = leveneTable,
                            logTransformed = logResult,
                            testResultData = comparisonResultsData$test,
                            cldData = comparisonResultsData$cld,
                            posthocData = comparisonResultsData$posthoc,
                            testName = testNames,
                            postHocName = postHocNames
                          ),envir = new.env(parent = globalenv())
                          )
      }
    )
    
    return(list(
      filter_data_stats = filter_data_stats,
      columnInput = columnInput,
      sampleInput = sampleInput,
      selected_stat = selected_stat,
      comparisonResults = comparisonResults,
      group_comparison = group_comparison,
      descriptives_table = descriptives_table
    ))
  })
}
