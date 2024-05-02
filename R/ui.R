library(shiny)
library(rmarkdown)
library(readr)
library(DT)
library(tidyverse)
library(datawizard)
library(purrr)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(bslib)
library(ggtext)
library(shinyjs)
library(car) 
library(multcomp) 
library(dplyr)
library(rlang)
library(broom)
library(DescTools)
library(FSA)
library(conover.test)
library(multcompView)

source("module_download.R")
source("utils_downloadGraphHandler.R")
source("about.R")
source("example_data_text.R")
source("utils_getColourSchemes.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "pulse"),
  tags$head(includeHTML("analytics.html")),
  includeCSS("www/style.css"),
  # tags$head(
  #   # Link to the CSS file
  #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  # ),
  # Use div to place the logo
  div(id = "logo", tags$img(src = "dottori_lab_pentagon.svg")),
  div(id = "logo2", tags$img(src = "UOW.png")),
  #shinyjs::useShinyjs(),
  # Application title
  div(tags$h1("FASTqPCR v1.1.0", style = "margin-left: 65px;")),
  sidebarLayout(
    sidebarPanel(
      style = "height: 85vh; overflow-y: auto;",
      # About tab
      conditionalPanel(condition = "input.tabselected==1", 
                       tags$h3("Contact"),
                       tags$p(HTML("If you have found this application useful please share with your networks. If there are any questions, or if you have any suggestions for improvement, please contact the 
<a href='mailto:mlm715@uowmail.edu.au?subject=FASTqPCR Feedback&body=Hi, I have some feedback for FASTqPCR:'>development team</a>, or raise an issue/request on 
<a href='https://github.com/MarnieMaddock/FASTqPCR/issues'>GitHub</a>.")),
                       tags$div(style = "height: 150px;"),
                       tags$div(tags$img(src = "dottori_lab.svg", height = "auto", width = 300), style = "text-align: center;")
      ),
      # Input Data Tab
      conditionalPanel(condition = "input.tabselected==2 && input.subInput == 2.1",
                       fileInput("file", "Choose CSV File", accept = c(".csv")),
                       helpText("Please select a CSV file containing PCR data with the formatting given in the Example Data Tab. This MUST have the same headings and Sample name structure."),
                       helpText("If you also have 'undetermined' amplification i.e. no amplification, please enter 0 in your dataset for those instances. Any NA values will be disregarded (i.e. REMOVED)."),
                        tags$br(),
                       numericInput(
                         "housekeepers",
                         "How many housekeeper genes do you have?",
                         3,
                         min = 1,
                         max = 10),
                       uiOutput("groups"),
                       helpText("Ensure that genes are entered exactly as they appear in the Target column."),
                       actionButton("save_btn", "Save housekeeper names"),
                       tags$br(),
                       tags$br(),
                       textOutput("text1"), verbatimTextOutput("saved_variables"),
                       tags$br(),
                       textOutput("text2")),
      #Calculations tab:
      conditionalPanel(condition = "input.tabselected == 3 && input.subPanel == 3.1",
                       h6("Note: This step will not work if there is a Sample that has two measurements of the same target. Everything must be unique."),
                       h6("Ensure that each sample has all housekeeper gene measurements (i.e. there are no missing values), or risk plotting the wrong data points."),
                       tags$br(),
                       helpText("mean_hk: Calculates the mean of the houskeepers you specified in the input data tab."),
                       tags$br(),
                       helpText("dcq_gene: Calculates the difference between the Cq value of your gene and the average of your housekeepers i.e. ΔCq = Cq (gene of interest) – Cq (housekeeping gene)."),
                       tags$br(),
                       helpText(HTML("fc_dcq_gene: Calculates the relative mRNA fold change - and is useful when you don't have an appropriate control/untreated reference value i.e. 2<sup>-(ΔCq)</sup>.")),
                       tags$br(),
                       helpText(HTML("dcq_ctrl_avg: Calculates the average of the control group for the selected gene.")),
                       helpText(HTML("ddcq_gene: Calculates the difference between the ΔCq of your gene and the average of the control group i.e. ΔΔCq = ΔCq (gene of interest) – ΔCq (control group).")),
                       tags$br(),
                       helpText(HTML("fc_ddcq: Fold change of the ΔΔCq value i.e. 2<sup>-(ΔΔCq)</sup>.")),
                       uiOutput("condition_filter")),
      conditionalPanel(
        condition = "input.tabselected == 3 && input.subPanel == 3.2 && input.subCalc2 == 3",
        fluidRow(
          column(width = 5, uiOutput("select_control")),
          column(width = 5, uiOutput("select_samples")),
          column(width = 5, uiOutput("column_selector2"))),
        actionButton("save_ddcq_data", "Save ΔΔCq Data"),
      ),
      conditionalPanel(condition = "input.tabselected == 5",
                       h4(HTML("<b>Create Graph</b>")),
                       radioButtons("select_dcq_or_ddcq", HTML("Select whether to graph 2<sup>-(ΔCq)</sup> or 2<sup>-(ΔΔCq)</sup>:"),
                                    choices = c("ΔCq" = "dcq", "∆ΔCq" = "ddcq"),
                                    selected = "dcq"),
                       uiOutput("selected_gene_ui"),
                       tags$br(),
                       fluidRow(
                         column(width = 6, uiOutput("condition_selector")),
                         column(width = 6, uiOutput("column_selector"))
                       ),
                       textInput("x_axis_positions", "Enter the order to display x-axis groups (comma-separated):", placeholder = "e.g., treated,untreated"),
                       helpText("Ensure spelling is exactly as it is entered in the Group column. Do NOT use spaces. e.g., untreated,treated"),
                       h4(HTML("<b>Customise Graph</b>")),
                       h5(HTML("<b>Labels</b>")),
                       uiOutput("x_axis_labels"),
                       # Checkbox for label rotation
                       checkboxInput("rotate_labels", "Rotate x-axis labels", value = FALSE),
                       uiOutput("dynamic_y_label_input"),
                       textInput("x_label", "Enter X-axis Label", value = "Group"),
                       selectInput("font_selector", "Select Font", choices = c("Arial", "Times New Roman", "Georgia", "Comic Sans MS", "Century Gothic", "Tahoma", "Verdana", "Courier New")),
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
                       
                       h5(HTML("<b>Graph Design</b>")),
                       selectInput("color_scheme_select", "Choose Colour Scheme:",
                                   choices = c("Custom" = "custom", "Colourblind friendly 1" = "colourblind1",
                                               "Colourblind friendly 2" = "colourblind2", "Colourblind friendly 3" = "colourblind3",
                                               "Colourblind friendly 4" = "colourblind4", "Colourblind friendly 5" = "colourblind5",
                                               "Grays 1" = "grays", "Grays 2" = "grays2",
                                               "Grays 3" = "grays3", "ElectraGray" = "electraGray", "Bones" = "bones",
                                               "Oranges 1" = "oranges", "Oranges 2" = "oranges2", "Pinks 1" = "pinks",
                                               "Pinks 2" = "pinks2", "Blues 1" = "blues", "Blues 2" = "blues2", "Greens 1" = "greens",
                                               "Greens 2" = "greens2", "Greens 3" = "greens3", "Green to Purple" = "green2purple",
                                               "Purples 1" = "purples", "Purples 2" = "purples3", "Purple to Orange" = "purple2orange",
                                               "Blaze" = "blaze", "Blaze 2" = "blaze2", "Peace 1" = "peace", "Peace 2" = "peace2", "Ireland" = "ireland",
                                               "Two-tone 1" = "twotone1", "Two-tone 2" = "twotone2", "Two-tone 3" = "twotone3",
                                               "Pastels 1" = "pastels", "Pastels 2" = "pastels2", "Pastels 3" = "pastels3", "Pastels 4" = "pastels4",
                                               "Pastels 5" = "pastels5", "Pastels 6" = "pastels6", "Pastels 7" = "pastels7", "Vibrant 1" = "vibrant",
                                               "Vibrant 2" = "vibrant2", "Vibrant 3" = "vibrant3", "Marnie's theme" = "marnie", "Marnie's theme 2" = "marnie2"),
                                   selected = "custom"),
                       selectInput("plot_type", "Choose Plot Type:",
                                   choices = c("Column Graph" = "column", "Dot Plot" = "dot"),
                                   selected = "column"),
                       h5(HTML("<b>Significance</b>")),
                       radioButtons("add_significance", "If statistics have been performed, add significance to graph.", choices = c("None" = "none", "Asterix Notation" = "asterix", "P Values" = "pval","Compact Letter Display" = "cld"),
                                    selected = "none"),
                       uiOutput("sigUI"),
                       h5(HTML("<b>Error Bars</b>")),
                       radioButtons("error_type", "Choose Error Bar Type:",
                                    choices = list("Standard Deviation" = "sd", 
                                                   "Standard Error" = "se"),
                                    selected = "se"),
                       # Add textInputs for custom Y-axis and X-axis labels
                       checkboxInput("start_at_zero", "Start Y-axis at 0: Note this may cut off data points/error bars close to zero.", value = TRUE)

                       
      ),
      # Add a dropdown menu for font selection
      conditionalPanel(condition = "input.tabselected == 5 && input.plot_type == 'column'",
                       fluidRow(
                         column(width = 6, numericInput("errorbar_width", "Error Bar Width:", value = 0.2, min = 0.05, max = 5, step = 0.1)),
                         column(width = 6, numericInput("errorbar_thickness", "Error Bar Thickness:", value = 1, min = 0.05, step = 0.2)),
                       ),
                       h5(HTML("<b>Point Design</b>")),
                       # Choose fill or colour
                       selectInput("fill_color_toggle", "Choose Fill or Border:",
                                   choices = c("Fill" = "fill", "Border" = "color"),
                                   selected = "fill"),
                       numericInput("dot_size", "Point Size:", value = 1.5, min = 1, max = 10, step = 0.5),
                       numericInput("dot_spacing", "Point Spacing:", value = 2.7, min = 0.1, max = 5, step = 0.1),

      ),
      conditionalPanel(
        condition = "input.tabselected == 5 && input.plot_type == 'dot'",
        fluidRow(
          column(width = 6, 
                 numericInput("error_bar_width", "Error Bar Width:", value = 0.2, min = 0.05, max = 5, step = 0.1),
                 numericInput("average_line_width", "Average Line Width:", value = 0.15, min = 0.01, step = 0.05)),
          column(width = 6, 
                 numericInput("error_bar_thickness", "Error Bar Thickness:", value = 1.5, min = 0.05, step = 0.2),
                 numericInput("average_line_thickness", "Average Line Thickness:", value = 1, min = 0.05, step = 0.1))
        ),
        h5(HTML("<b>Point Design</b>")),
        numericInput("point_size", "Point Size:", value = 3, min = 1, max = 10, step = 0.5),
        checkboxInput("change_shapes", "Change to paired shapes.", FALSE),
        fluidRow(
          column(width = 6, numericInput("stroke_thickness", "Shape Outline Thickness", value = 1.5, min = 0.1, step = 0.2)),
          column(width = 6, numericInput("jitter_amount", "Point Spread:", value = 0.2, min = 0, max = 1.5, step = 0.1)),
        ),
        numericInput("seed_input", "Set Seed:", value = 123),
        helpText("This is a random value that allows you to change the order of the points on your graph. Change the number if points overlap for example."),

        tags$br(),
        tags$br(),
      ),
      conditionalPanel(condition = "input.tabselected == 4 && input.subStats == 1",
                       h3(HTML("<b>Statistical Flowchart</b>")),
                       h6("This is the suggested workflow for statistical analysis of PCR data. This is a guide only and should be used in conjunction with your own knowledge of statistics for your own applications."),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       h6("Note, the choice of small sample size being < 12 is an arbitrary value. Use the context of your question and your field of study to determine what constitutes a small sample size.")),
      conditionalPanel(condition = "input.tabselected == 4 && input.subStats == 2",
                       h5(HTML("<b>Statistics</b>")),
                       radioButtons("select_dcq_or_ddcq_stats", HTML("Select whether to use 2<sup>-(ΔCq)</sup> or 2<sup>-(ΔΔCq)</sup> for statistical tests:"),
                                    choices = c("ΔCq" = "dcq_stats", "∆ΔCq" = "ddcq_stats"),
                                    selected = "dcq_stats"),
                       h6("Select the samples and gene to perform statistics on."),
                       selectInput("sampleInput", "Select Sample:", choices = NULL, multiple = TRUE),
                       uiOutput("ddcqMessage"),
                       uiOutput("selected_gene_ui_stats"),
                       selectInput("columnInput", "Select Gene:", choices = NULL),
                       tags$br(),
                       h5(HTML("<b>Select the statistical tests to perform.</b>")),
                       h6(HTML("<b>1. Sample Size</b>")),
                       checkboxInput("sample_size", "Calculate sample size", value = FALSE),
                       checkboxGroupInput("normality_test", HTML("<b>2. Select normality test:</b>"), choices = c("Shapiro-Wilk" = "shapiro", "QQ-Plot" = "qqplot", "Density Plot" = "density"),
                                          selected = NULL),
                       #verbatimTextOutput("testResults"),
                       h6(HTML("<b>3. Homogeneity of Variance</b>")),
                       checkboxInput("variance", "Check for equal variance (Levene's test).", value = FALSE),
                       h6(HTML("<b>4. Log transform data? (Recommended for data that is NOT normally distributed/unequal variance)</b>")),
                       checkboxInput("log_transform", "Log10", value = FALSE),
                       helpText("Note: If you have a small sample size, it is recommended to use non-parametric tests (even if the data is normally distributed)."),
                       radioButtons("group_comparison", HTML("<b>5. Select the group comparisons to perform:</b>"), choices = c("Parametric Test" = "parametric", "Non-parametric Test" = "non_parametric"),
                                          selected = character(0)),
                       uiOutput("postHocOptions"),
                       uiOutput("correctionOptions"),
                       uiOutput("downloadButtonUI"),
                       verbatimTextOutput("shapiroOutput")),
    ),
    
    
    mainPanel(
      tabsetPanel(
        type = "tabs", 
        id = "tabselected", 
        selected = 1, # Default tab selected is 1
        tabPanel("About", icon = icon("home", lib = "font-awesome"), textOutput("about"), value = 1,
                 about_text),
        tabPanel("Input Data", textOutput("inputdata"), value = 2, 
                 tabsetPanel(
                   id = "subInput",
                   selected = 2.1,
                   tabPanel("Data", value = 2.1,
                            h3(HTML("Inserted Data")),
                            # Display uploaded data using DataTable
                            dataTableOutput("table"),
                            tags$br(),
                            tags$br()),
                   tabPanel("Example Data", value = 2.2,
                            example_data_text
                            )
                 )
                ),
        tabPanel("Calculations", value = 3,
                 tabsetPanel(
                   id = "subPanel",
                   selected = 3.1,
                   tabPanel(HTML("2<sup>-(∆Cq)</sup>"), value = 3.1,
                            tabsetPanel(
                              id = "subCalc",
                              selected = 1,
                              tabPanel("All Data", value = 1,
                                       h4(HTML("<b>Average the houskeeping genes and perform ∆Cq and 2<sup>-∆Cq</sup></b>")),
                                       tags$br(),
                                       dataTableOutput("calculations_table"),
                                       # Add this inside your UI, preferably in the "Calculations" tabPanel
                                       downloadUI("download_processed_data", "Download Processed Data"),
                                       tags$br(),
                                       tags$br(),
                                       h4(HTML("<b>Filter by Condition</b>")),
                                       dataTableOutput("filtered_table"),
                                       downloadUI("download_filtered_data", "Download Filtered Data"),
                                       tags$br(),
                                       tags$br()),
                              tabPanel("Biological Replicate", value = 2,
                                       h4(HTML("<b>Biological Replicate Average Values</b>")),
                                       tags$br(),
                                       dataTableOutput("rep_avg_table"),
                                       downloadUI("download_rep_avg_data", "Download Replicate Average Data"),
                                       tags$br(),
                                       tags$br(),
                                       h4(HTML("<b>Filter by Condition</b>")),
                                       dataTableOutput("rep_avg_filtered_table"),
                                       downloadUI("download_rep_avg_filtered_data", "Download Filtered Replicate Average Data"),
                                       tags$br(),
                                       tags$br())
                            )
                   ),
                   tabPanel(HTML("2<sup>-(∆∆Cq)</sup>"), value = 3.2,
                            tabsetPanel(
                              id = "subCalc2",
                              selected = 3,
                              tabPanel("All Data", value = 3,
                                       h4(HTML("<b>Average ∆Cq for control and perform 2<sup>-∆ΔCq</sup></b>")),
                                       dataTableOutput("ddcq_data"),
                                       downloadUI("download_ddcq_data", "Download Processed Data"),
                                       tags$br()),
                              tabPanel("Biological Replicate", value = 4,
                                       h4(HTML("<b>Biological Replicate Average Values</b>")),
                                       dataTableOutput("rep_avg_table_ddcq"),
                                       downloadUI("download_ddcq_avg_data", "Download Replicate Data"))
                            )
                   ),
                 ),
        ),
        
        tabPanel("Stats", value = 4,
                 tabsetPanel(
                   id = "subStats",
                   selected = 1,
                   tabPanel("Flowchart", value = 1,
                            h4(HTML("<b>Statistical Flowchart</b>")),
                            tags$img(src = "stats_flowchart.svg", height = 550, width = 750),
                            tags$br(),
                            tags$br(),
                            tags$br()),
                   tabPanel("Statistics", value = 2,
                            uiOutput("sampleSizeHeading"),
                            dataTableOutput("nTable"),
                            tags$br(),
                            h6("Note for small sample sizes, it is recommended to use non-parametric tests (even if the data is normally distributed)."),
                            uiOutput("normalityHeading"),
                            uiOutput("normalityTableUI"),
                            uiOutput("qqPlotUI"),
                            uiOutput("densityPlotUI"),
                            uiOutput("leveneHeading"),
                            uiOutput("leveneUI"),
                            uiOutput("comparisonsHeading"),
                            uiOutput("testResultTable"),
                            uiOutput("postHocHeading"),
                            uiOutput("postHocTableUI"),
                            uiOutput("cldHeading"),
                            uiOutput("cld_tableUI"),
                            tags$br(),
                            tags$br()
                   )
                 )
        ),
        tabPanel("Graphs", value = 5,
                 #graphing_mainUI("main"),
                 # Add a plot
                 plotOutput("plot"),
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
                   column(4,
                          # Add a download button
                          downloadButton("downloadGraph", "Download Graph")
                   ),
                   column(4,
                          # Add a download button
                          downloadButton("downloadGraphOptions", "Download Selected Graph Options")
                   )
                 ),
                 tags$br(),
                 p(HTML("Note: Colour Scheme suggestions for graphs can be sent to <a href='mailto:mlm715@uowmail.edu.au?subject=FASTqPCR Colour Request&body=Hi, I have a colour scheme request. Here is a list of colours in order. Please use hex codes, e.g. #000000, #63b8ff:'>Marnie</a>.")),
                 tags$br(),
                 tags$br()
                ),
      )
    )
  )        
)        