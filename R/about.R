library(shiny)


# Define a function to get the correct file path
get_footer_path <- function() {
  if (file.exists("inst/www/footer.svg")) {
    return("inst/www/footer.svg")
  } else {
    return(system.file("www", "footer.svg", package = "ProntoPCR"))
  }
}

#text that is displayed in the about tab of the app
about_text <- div(
  h3("About ProntoPCR"),
  p("Pronto PCR has been developed to simplify and accelerate the analysis of qPCR data, including performing calculations, statistics and graphing (which is known to be a repetitive and time-consuming task). The application is designed to do the following:"),
  shiny::tags$ul(
    tags$li("Instant calculations such as averaging the housekeeper genes, calculating ΔCq, ΔΔCq and fold change (i.e. 2^-ΔCq / 2^-ΔΔCq)"),
    tags$li("Perform statistical analysis such as testing for normality, homogeneity of variance, performing parametric/non-parametric tests for comparing groups, and giving mulitple options for post-hoc analysis"),
    tags$li("Graph the data and alter the aesthetics to the users preferences. There is also the option to add the results of the statistical analysis to the graph")
  ),
  p("All calculations can be downloaded as a .csv file if the user wishes to analyse the data in another application. 
      All graphs also have the option to be saved as an .svg so that aesthetics can be further modified in other programs such as Illustrator and Inkscape (which is free!).
      Additionally, all statistics and analysis can be saved as a report in .html format."),
  p("We request that users of ProntoPCR cite the associated journal article if the application has been used in analysis. The citation is as follows:"),
  p(HTML('Please refer to the <a href="https://github.com/MarnieMaddock/ProntoPCR/blob/main/ProntoPCR_Handbook.docx" target="_blank">user guide</a> for more information on how to use the application.')),
  shiny::tags$br(),
  shiny::tags$br(),
  shiny::tags$br(),
  #add footer image. Dynamic due to differences in format requirements for running online through shinyapps.io vs locally from github
  div(id = "footer", bslib::card_image(file = get_footer_path(), fill = FALSE, width = "800px")),
  shiny::tags$br(),
  shiny::tags$br(),
  shiny::tags$br()
)
