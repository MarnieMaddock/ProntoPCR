# module_statsWorkflow.R

workflowSidebar <- function(id) {
  ns <- NS(id)
  tagList(
    h3(HTML("<b>Statistical Analysis Workflow</b>")),
    h6("Note: There is no one-size-fits-all approach to statistical analysis. The workflow below is a starting point for analysis of PCR data. This is a guide only and should be used in conjunction with good knowledge of statistics for your own applications."),
    tags$br(),
    tags$br(),
    tags$br(),
    h6("Note, defining a small sample size is subjective and depends on the context of your question and your field of study. In this workflow, the sample size of 6 or fewer being considered as a small sample size was an arbirtray decision."),
    tags$br(),
    #insert link to download manual
    h6("For more information, please refere to the ProntoPCR Manual:")
  )
}

workflowMain <- function(id) {
  ns <- NS(id)
  tagList(
    tags$img(src = "stats_flowchart.svg", height = 550, width = 750),
    tags$br(),
    tags$br(),
    tags$br(),
  )
}