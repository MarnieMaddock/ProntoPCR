about_text <- div(
    h3("About STATqPCR"),
    p("STATqPCR has been developed to simplify and accerlate the analysis of qPCR data (which is known to be a repetitive and time-consuming task). All caluclations, statistics and graphing can be performed in under 1 minute*.
      The web application is designed to do the following:"),
    shiny::tags$ul(
      tags$li("Instant calculations such as averaging the housekeeper genes, calculating ΔCq, ΔΔCq and fold change (i.e. 2^-ΔCq/2^-ΔΔCq)"),
      tags$li("Perform statistical analysis such as testing for normality, homogeneity of variance, performing parametric/non-parametric tests for comparing groups, and giving mulitple options for post-hoc analysis"),
      tags$li("Graph the data and alter the aesthetics to the users preferences. There is also the option to add the results of the statistical analysis to the graph"),
    ),
    p("Calculations can be downloaded as a .csv file if the user wishes to analyse the data in another application. 
      All graphs also have the option to be saved as an .svg so that aesthetics can be further modified in other programs such as Illustrator and Inkscape (which is free!).
      Additionally, all statistics and analysis can be saved as a report in .pdf format (which is useful for writing up result sections)."),
    
    h5("Preparing your data"),
    p("For STATqPCR to work, the data must be in a specific (but simple) format. The data must be in a .csv file. Note this is compatible directly with the output of QuantStudio Design and Analysis Software Results (just go to the Quality Check tab, selecte Replicate Group on the right hand side, select the three dots and export the data:"),
    tags$img(src = "quantstudio.svg", height = "350px", width = "auto"),
    p("The data columns should include (ensure correct spelling, capitalisations and no spaces - use . instead):"),
    shiny::tags$ul(
      tags$li("Sample"),
      tags$li("Target"),
      tags$li("Num.of.Replicates"),
      tags$li("Cq.Mean"),
      tags$li("Cq.SD"),
      tags$li("Result.Quality.Issues"),
    ),
    p("The Sample column should be formatted as follows:"),
    tags$img(src = "Sample.svg", height = "200px", width = "auto"),
    p("Where grouping1 is your main group that you are comparing e.g. diseased and control cell lines, or different drugs, the number refers to the biological replicate number, and grouping2 is an additional grouping factor e.g. different cell types. A second grouping is not a requirement, and can be combined into grouping1 e.g. DrugA1uM. Please write NIL if there is not second grouping variable, do not leave this blank.
      Note: If you set up your PCR experiments using this format, this step is very easy. E.g. Plate set-up of a PCR experiment (In Quantstudio software)."),
    tags$img(src="plate_setup.svg", height = "300px", width = "auto"),
      # If you are unsure of how to format your data, please refer to the example data provided in the 'Example Data' tab. 
      # If you are still unsure, please refer to the 'Contact' tab."),
    p("Refer to Input Data Tab for another example."),
    
    p("*Once the user is familar with the user interface, you can expect to perform all calculations and analysis in under 1 minute. This estimate does not include the time it takes to pre-format and load the data."),
  )
