library(shiny)
library(readr)
library(DT)
library(tidyverse)
library(datawizard)
library(purrr)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(bslib)
library(shinyjs)

source("module_download.R")
source("utils_downloadGraphHandler.R")
source("utils_graphPanel.R")
source("utils_graphDisplay.R")
#source("graphing_module.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "pulse"),
  shinyjs::useShinyjs(),
  # Application title
  titlePanel("STATqPCR"),

  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      style = "height: 85vh; overflow-y: auto;",
      # About tab
      conditionalPanel(condition = "input.tabselected==1", tags$img(src = "Dottori_lab.svg", height = 300, width = 350)),
      # Input Data Tab
      conditionalPanel(condition = "input.tabselected==2",
                       fileInput("file", "Choose CSV File", accept = c(".csv")),
                       helpText("Please select a CSV file containing PCR data with the formatting given on the right. This MUST have the same headings and Sample name structure."),
                       helpText("If you also have 'undetermined' amplification i.e. no amplification, please enter 0 in your dataset for those instances. Any NA values will be disregarded."),
                       helpText("Please ensure you have no gaps between rows in your dataset. Each row should have at least one measurement."),
                       numericInput(
                         "housekeepers",
                         "How many housekeeper genes do you have?",
                         3,
                         min = 1,
                         max = 10),
                       uiOutput("groups"),
                       actionButton("save_btn", "Save housekeeper names"),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       textOutput("text1"), verbatimTextOutput("saved_variables"),
                       tags$br(),
                       textOutput("text2")),
      conditionalPanel(condition = "input.tabselected== 3 && input.subPanel == 3.1",
                       h6("Note: This step will not work if there is a Sample that has two measurements of the same target. Everything must be unique."),
                       h6("Ensure that each sample has all housekeeper gene measurements (i.e. there are no missing values), or risk plotting the wrong data points."),
                       tags$br(),
                       helpText("mean_hk: Calculates the mean of the houskeepers you insterted in the input data tab."),
                       tags$br(),
                       helpText("dct_genename: Calculates the difference between the Ct value of your gene and the average of your housekeepers i.e. ΔCt = Ct (gene of interest) – Ct (housekeeping gene)."),
                       tags$br(),
                       helpText(HTML("fc_dct_genename: Calculates the relative mRNA fold change - and is useful when you don't have an appropriate control/untreated reference value i.e. 2<sup>-(ΔCt)</sup>.")),
                       tags$br(),
                       helpText(HTML("Note: 2<sup>-(∆∆Ct)</sup> will be performed in the ∆∆Ct tab upon following the instructions.")),
                       tags$br(),
                       tags$style(HTML(".custom-break { height: 300px; }")),
                       tags$br(),
                       tags$div(class = "custom-break"),
                       uiOutput("condition_filter")
                       
      ),
      conditionalPanel(
        condition = "input.tabselected == 3 && input.subPanel == 3.2",
        graphPanel()
      ),
      conditionalPanel(
        condition = "input.tabselected == 3 && input.plot_type == 'dot'",
        numericInput("jitter_amount", "Point Spread:", value = 0.2, min = 0, max = 1.5, step = 0.1),
        numericInput("seed_input", "Set Seed:", value = 123),
        helpText("This is a random value that allows you to change the order of the points on your graph. Change the number if points overlap for example."),
        tags$br(),
        tags$br()
      ),
      conditionalPanel(condition = "input.tabselected== 4 && input.subPanel2 == 4.1",
                       fluidRow(
                         column(width = 4, uiOutput("select_control")),
                         column(width = 6, uiOutput("select_samples")),
                         column(width = 4, uiOutput("select_condition")),
                         column(width = 6, uiOutput("column_selector2"))
                       ),
                       
      ),
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs", 
        id = "tabselected", 
        selected = 1, # Default tab selected is 1
        tabPanel("About", icon = icon("home", lib = "font-awesome"), textOutput("about"), value = 1),
        tabPanel("Input Data", textOutput("inputdata"), value = 2, tags$img(src = "table.png", height = 400, width = 600),
                 # Display uploaded data using DataTable
                 dataTableOutput("table"),
                 tags$br(),
                 tags$br()
        ),
        
        tabPanel(HTML("2<sup>-(∆Ct)</sup>"), value = 3,
                 tabsetPanel(
                   id = "subPanel", 
                   selected = 3.1,
                   tabPanel("Calculations", value = 3.1,
                            tabsetPanel(
                              id = "CalcSubPanel",
                              selected = 3.1,
                              tabPanel("All Data", value = 3.1,
                                       h4(HTML("<b>Average the houskeeping genes and perform ∆Ct and 2<sup>-∆Ct</sup></b>")),
                                       tags$br(),
                                       dataTableOutput("calculations_table"),
                                       # Add this inside your UI, preferably in the "Calculations" tabPanel
                                       downloadDataUI("downloads_data"),
                                       tags$br(),
                                       tags$br(),
                                       h4(HTML("<b>Filter by Condition</b>")),
                                       dataTableOutput("filtered_table"),
                                       downloadFilteredDataUI("downloads_filtered_data"),
                                       tags$br(),
                                       tags$br(),
                              ),
                              tabPanel("Biological Replicate Data", value = 3.2,
                                       h4(HTML("<b>Biological Replicate Average Values</b>")),
                                       tags$br(),
                                       dataTableOutput("rep_avg_table"),
                                       downloadRepAvgDataUI("downloads_rep_avg_data"),
                                       tags$br(),
                                       h4(HTML("<b>Filter by Condition</b>")),
                                       dataTableOutput("rep_avg_filtered_table"),
                                       downloadRepAvgFilteredDataUI("downloads_rep_avg_filtered_data"),
                                       tags$br(),
                                       tags$br())
                            )),
                   tabPanel("Graphs", value = 3.2,
                            div(
                              # Add a plot
                              plotOutput("plot"),
                              tags$br(),
                              tags$br(),
                              tags$br(),
                              tags$br(),
                              tags$br(),
                              h4(HTML("Download Graph")),
                              h6("SVG graphs are editable in illustrator, inkscape etc."),
                              fluidRow(
                                column(4,
                                       selectInput("file_format", "Choose File Format:",
                                                   choices = c("svg", "png", "jpeg", "tiff"),
                                                   selected = "svg")
                                ),
                                column(4,
                                       numericInput("dpi", "DPI:", 500)
                                ),
                                column(2, 
                                       numericInput("width", "Width (inches):", 8)
                                ),
                                column(2,
                                       numericInput("height", "Height (inches):", 5)
                                ),
                                column(12,
                                       # Add a download button
                                       downloadButton("downloadGraph", "Download Graph") 
                                )
                              )
                            ),
                            tags$br(),
                            tags$br()),
                   tabPanel("Stats", value = 3.3),
                   tabPanel("Graphs & Stats", value = 3.4)
                 )
        ),
        
        tabPanel(HTML("2<sup>-(∆∆Ct)</sup>"), value = 4,
                 tabsetPanel(
                   id = "subPanel2",
                   selected = 4.1,
                   tabPanel(
                     "Calculations", value = 4.1,
                     dataTableOutput("ddct_data")
                   ),
                   tabPanel(
                     "Graphs", value = 4.2),
                   tabPanel(
                     "Stats", value = 4.3),
                   tabPanel(
                     "Graphs & Stats", value = 4.4
                   )
                 )
        )
        
      )
    )
  )
)