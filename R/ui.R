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



# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "pulse"),
  shinyjs::useShinyjs(),
  # Application title
  titlePanel("STATqPCR"),
  tags$h6("For when you want your PCR data graphed and analysed STAT"),
  
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
      conditionalPanel(condition = "input.tabselected == 3 && input.subPanel == 3.2",
                       h5(HTML("<b>Create Graph</b>")),
                       fluidRow(
                         column(width = 6, uiOutput("condition_selector")),
                         column(width = 6, uiOutput("column_selector"))
                       ),
                       textInput("x_axis_positions", "Enter the order to display X-Axis categories (comma-separated):", placeholder = "e.g., untreated,treated"),
                       helpText("Ensure spelling is exactly as it is entered in the Group column. Do NOT use spaces."),
                       h5(HTML("<b>Customise Graph</b>")),
                       # Add textInputs for custom Y-axis and X-axis labels
                       textInput("y_label", "Enter Y-axis Label", value = "Relative GENE NAME mRNA (2^-ΔCq)"),
                       textInput("x_label", "Enter X-axis Label", value = "Group"),
                       selectInput("font_selector", "Select Font", choices = c("Arial", "Times New Roman", "Helvetica", "Georgia", "Comic Sans MS", "Century Gothic",  "Courier New")),
                       fluidRow(
                         column(
                           width = 6,
                           numericInput("x_axis_title_font_size", "X-axis Title Font Size:", value = 14, min = 1, max = 50),
                           numericInput("x_axis_label_font_size", "X-axis Text Font Size:", value = 12, min = 1, max = 50)
                         ),
                         column(
                           width = 6,
                           numericInput("y_axis_title_font_size", "Y-axis Title Font Size:", value = 14, min = 1, max = 50),
                           numericInput("y_axis_label_font_size", "Y-axis Text Font Size:", value = 12, min = 1, max = 50)
                         )
                       ),
                       tags$br(),
                       uiOutput("x_axis_labels"),
                       # Checkbox for label rotation
                       checkboxInput("rotate_labels", "Rotate x-axis labels", value = FALSE),
                       selectInput("plot_type", "Choose Plot Type:",
                                   choices = c("Column Graph" = "column", "Dot Plot" = "dot"),
                                   selected = "column"),
                       selectInput("color_scheme_select", "Choose Colour Scheme:",
                                   choices = c("Custom" = "custom", "Colourblind friendly 1" = "colourblind1", 
                                               "Colourblind friendly 2" = "colourblind2", "Colourblind friendly 3" = "colourblind3",
                                               "Colourblind friendly 4" = "colourblind4", "Grays 1" = "grays", "Grays 2" = "grays2",
                                               "Grays 3" = "grays3", "ElectraGray" = "electraGray", "Bones" = "bones", 
                                               "Oranges 1" = "oranges", "Oranges 2" = "oranges2", "Pinks 1" = "pinks", 
                                               "Pinks 2" = "pinks2", "Blues 1" = "blues", "Blues 2" = "blues2", "Greens 1" = "greens",
                                               "Greens 2" = "greens2", "Greens 3" = "greens3", "Green to Purple" = "green2purple",
                                               "Purples 1" = "purples", "Purples 2" = "purples3", "Purple to Orange" = "purple2orange",
                                               "Blaze" = "blaze", "Blaze 2" = "blaze2", "Peace 1" = "peace", "Peace 2" = "peace2", "Ireland" = "ireland",
                                               "Pastels 1" = "pastels", "Pastels 2" = "pastels2", "Pastels 3" = "pastels3", "Pastels 4" = "pastels4",
                                               "Pastels 5" = "pastels5", "Pastels 6" = "pastels6", "Pastels 7" = "pastels7", "Vibrant 1" = "vibrant", 
                                               "Vibrant 2" = "vibrant2", "Vibrant 3" = "vibrant3"),
                                   selected = "custom"),
                       numericInput("dot_size", "Point Size:", value = 1.5, min = 1, max = 10, step = 0.5),
                       # Add a dropdown menu for font selection
                       
                       
                       
                       
                       
                       conditionalPanel(
                         condition = "input.plot_type == 'column'",
                         # Choose fill or colour
                         selectInput("fill_color_toggle", "Choose Fill or Border:",
                                     choices = c("Fill" = "fill", "Border" = "color"),
                                     selected = "fill"))
                       
      ),
      conditionalPanel(condition = "input.tabselected== 3 && input.plot_type == 'dot'",
                       numericInput("jitter_amount", "Point Spread:", value = 0.2, min = 0, max = 1.5, step = 0.1),
                       numericInput("seed_input", "Set Seed:", value = 123),
                       helpText("This is a random value that allows you to change the order of the points on your graph. Change the number if points overlap for example."),
                       tags$br(),
                       tags$br()
      ),
      conditionalPanel(condition = "input.tabselected== 4 && input.subPanel2 == 4.1",
                       fluidRow(
                         column(width = 6, uiOutput("select_control")),
                         column(width = 6, uiOutput("select_samples")),
                         column(width = 6, uiOutput("select_condition")),
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
        #Calculations tab
        
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
                                       downloadButton("downloadData", "Download Processed Data"),
                                       tags$br(),
                                       tags$br(),
                                       h4(HTML("<b>Filter by Condition</b>")),
                                       dataTableOutput("filtered_table"),
                                       downloadButton("downloadFilteredData", "Download Filtered Data"),
                                       tags$br(),
                                       tags$br(),
                              ),
                              tabPanel("Biological Replicate Data", value = 3.2,
                                       h4(HTML("<b>Biological Replicate Average Values</b>")),
                                       tags$br(),
                                       dataTableOutput("rep_avg_table"),
                                       downloadButton("rep_avg_download", "Download Replicate Average Data"),
                                       tags$br(),
                                       h4(HTML("<b>Filter by Condition</b>")),
                                       dataTableOutput("rep_avg_filtered_table"),
                                       downloadButton("rep_avg_filtered_download", "Download Filtered Replicate Average Data"),
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