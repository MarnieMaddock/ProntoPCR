
exampleDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Example CSV File"),
    tags$img(src = "www/exampleTable.png", height = 400, width = 680),
    tags$br(),
    tags$br(),
    tags$br(),
    h4("Checklist:"),
    div(
      tags$input(type = "checkbox", id = "option1"), tags$label("CSV file containing PCR data with the formatting given above?", `for` = "option1"),
      tags$br(),
      tags$input(type = "checkbox", id = "option2"), tags$label("Column names have capital letters when specified?", `for` = "option2"),
      tags$br(),
      tags$input(type = "checkbox", id = "option3"), tags$label("Column names have full-stops . when specified?", `for` = "option3"),
      tags$br(),
      tags$input(type = "checkbox", id = "option4"), tags$label("There are no spaces present in the column names?", `for` = "option4"),
      tags$br(),
      tags$input(type = "checkbox", id = "option5"), tags$label("There are no gaps between rows in your dataset. Each row should have at least one measurement.", `for` = "option5"),
      tags$br(),
      tags$input(type = "checkbox", id = "option6"), tags$label("If you have 'undetermined' amplification i.e. no amplification, 0 has been entered in your dataset for those instances. Any NA values will be disregarded.", `for` = "option6"),
      tags$br(),
      tags$input(type = "checkbox", id = "option7"), tags$label("All Sample names have no spaces, and are using underscores (_) in the naming system.", `for` = "option7"),
      tags$br(),
      tags$input(type = "checkbox", id = "option8"), tags$label("All non-template controls (NTC) and no reverse transcriptase controls (-RT) have been removed.", `for` = "option8"),
      tags$br(),
      tags$input(type = "checkbox", id = "option9"), tags$label("Persisting errors/no data loading? Contact Marnie: mlm715@uowmail.edu.au.", `for` = "option9"),
    )
  )
}



