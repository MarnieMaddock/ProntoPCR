#module_logStats.R#
# log transform sidebar
logSidebar <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("log_transform"), label = HTML(
      paste0(
        "<b>4. Log transform data:</b> ",
        tags$i(
          class = "glyphicon glyphicon-info-sign",
          style = "color:#00359bff;",
          title = "Applying a log transformation can help normalize data and reduce skewness if the data is not normally distributed or has unequal variance."
        )
        )
      ), choices = c("Log2", "Log10", "None"), selected = "None")
  )
}


logServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive expression to track the state of the checkbox
    log_transform_state <- reactive({
      input$log_transform
    })
    
    # Return the reactive expression
    return(log_transform_state)
    
  })
}