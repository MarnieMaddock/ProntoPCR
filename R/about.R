library(shiny)
about_text <- div(
  h3("About ProntoPCR"),
  p("Pronto PCR has been developed to simplify and accelerate the analysis of qPCR data, including performing calculations, statistics and graphing (which is known to be a repetitive and time-consuming task). The application is designed to do the following:"),
  shiny::tags$ul(
    tags$li("Instant calculations such as averaging the housekeeper genes, calculating ΔCq, ΔΔCq and fold change (i.e. 2^-ΔCq / 2^-ΔΔCq)"),
    tags$li("Perform statistical analysis such as testing for normality, homogeneity of variance, performing parametric/non-parametric tests for comparing groups, and giving mulitple options for post-hoc analysis"),
    tags$li("Graph the data and alter the aesthetics to the users preferences. There is also the option to add the results of the statistical analysis to the graph"),
  ),
  p("All calculations can be downloaded as a .csv file if the user wishes to analyse the data in another application. 
      All graphs also have the option to be saved as an .svg so that aesthetics can be further modified in other programs such as Illustrator and Inkscape (which is free!).
      Additionally, all statistics and analysis can be saved as a report in .html format."),
  p("We request that users of ProntoPCR cite the associated journal article if the application has been used in analysis. The citation is as follows:"),
  p("Please refer to the user guide and video tutorial for more information on how to use the application."),
  shiny::tags$br(),
  shiny::tags$br(),
  shiny::tags$br(),
  #add footer image
  div(id = "footer", bslib::card_image(src = "www/footer.svg", fill = FALSE, width = "800px")),
  shiny::tags$br(),
  shiny::tags$br(),
  shiny::tags$br()
)
