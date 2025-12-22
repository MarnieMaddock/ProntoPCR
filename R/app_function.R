#' Run the ProntoPCR Shiny Application
#'
#' This function launches the ProntoPCR Shiny app.
#' @param ... Additional arguments to pass to the ProntoPCR function.
#' @name ProntoPCR
#' @import shiny
#' @import ggtext
#' @import bslib
#' @importFrom bslib bs_theme
#' @importFrom shinyjs useShinyjs
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @importFrom dplyr case_when
#' @importFrom dplyr n
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_jitter
#' @importFrom ggplot2 position_jitter
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 expansion
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 mean_se
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 stat_summary
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 last_plot
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom ggpubr stat_pvalue_manual


# Define a functions to get the correct file paths depending on whether app is run in shinyapps.io or locally from github
get_css_path <- function() {
  if (file.exists("inst/www/style.css")){
    return("inst/www/style.css") #shinyapps.io
  } else {
    return(system.file("www", "style.css", package = "ProntoPCR")) #github
  }
}

get_logo_path <- function() {
  if (file.exists("inst/www/dottori_lab_pentagon.svg")) {
    return("inst/www/dottori_lab_pentagon.svg") #shinyapps.io
  } else {
    return(system.file("www", "dottori_lab_pentagon.svg", package = "ProntoPCR")) #github
  }
}

get_UOW_path <- function() {
  if (file.exists("inst/www/UOW.png")) {
    return("inst/www/UOW.png") #shinyapps.io
  } else {
    return(system.file("www", "UOW.png", package = "ProntoPCR")) #github
  }
}

# get_font_path <- function(file) {
#   if (file.exists(paste0("inst/www/", file))) {
#     return(paste0("inst/www/", file))
#   } else {
#     return(paste0("www/", file))
#   }
# }

get_font_path <- function(file) {
  system.file("www", file, package = "ProntoPCR")
}

include_analytics_html <- function() {
  # Check if the file exists locally or for shinyapps.io
  file_path <- if (file.exists("inst/www/analytics.html")) {
    "inst/www/analytics.html"
  } else if (file.exists("www/analytics.html")) {
    # Use relative path for shinyapps.io
    "www/analytics.html"
  } else {
    NULL
  }
  
  # Include the HTML file in the app only if the file path is valid
  if (!is.null(file_path)) {
    tags$head(includeHTML(file_path))
  } else {
    NULL
  }
}


#' @export
ProntoPCR <-  function(...) {
  #only perform if app is run locally
  if(Sys.getenv("RSCONNECT_SERVER") == ""){
    check_for_updates()
    # Display formal citation message in console
    message("\n==================================================================",
            "\n📖 If you use ProntoPCR for data analysis or in a journal article,",
            "\nplease cite the following reference:",
            "\n",
            "\n📌 Citation Here",
            "\n",
            "\nThank you for supporting open-source research!",
            "\n==================================================================\n")
  }
  
  #set up UI 
  ui <- fluidPage(
    theme = bslib::bs_theme(version = 4, bootswatch = "pulse"), #theme
    include_analytics_html(), # Include Google Analytics
    tags$head(includeCSS(get_css_path())), # Use custom CSS for UI
    # Use bslib::card_image to include images
    div(id = "logo", bslib::card_image(file = get_logo_path(), fill = FALSE, width = "70px")),
    div(id = "logo2", bslib::card_image(file = get_UOW_path(), fill = FALSE, width = "220px")),
    
    # Application title
    div(tags$h1("ProntoPCR v1.0.0", style = "margin-left: 65px;")),
    shinyjs::useShinyjs(), # Use shinyjs to hide and show elements
    #sidebar options
    sidebarLayout(
      sidebarPanel(
        style = "height: 85vh; overflow-y: auto;", # Set the sidebar height and add a scroll bar
        conditionalPanel(condition = "input.tabselected==2 && input.subInput == 2.1",
                         inputFileUI("file")
        ), #insert csv file and check that it meets the required formatting, enter housekeeper names and save them
        #Calculations tab: #Delta Cq tab
        conditionalPanel(condition = "input.tabselected == 3 && input.subPanel == 3.1",
                         wrangleDataSidebar("wrangleDataModule") #display delta Cq data, average housekeepers module
        ),
        conditionalPanel(condition = "input.tabselected == 3 && input.subPanel == 3.2 && input.subCalc2 == 3",
                         ddcqSidebar("ddcqModule") #display ddcq data module
        ),
        # Statistics tab
        # Suggested workflow tab
        conditionalPanel(condition = "input.tabselected == 4",
                         statsSidebar("statsModule") #display stats options module
        ),
        conditionalPanel(condition = "input.tabselected == 5",
                         graphsSidebar("graphsModule") #display graphs options
        ),
      ), #sidebarPanel closing bracket
      
      mainPanel(
        tabsetPanel(
          type = "tabs", 
          id = "tabselected", 
          selected = 1, # Default tab selected is 1
          tabPanel("About", icon = icon("home", lib = "font-awesome"), #display home icon in the tab
                   textOutput("about"), value = 1,
                   about_text
          ), #display about text from source("about.R")
          tabPanel("Input Data", textOutput("inputdata"), value = 2,
                   tabsetPanel(
                     id = "subInput",
                     selected = 2.1, #display inserted data by the user
                     tabPanel("Data", value = 2.1,
                              #Display uploaded data using DataTable (module_deltaCq.R)
                              inputDataUI("inputDataModule"),
                     ),
                     tabPanel("Example Data", value = 2.2,
                              exampleDataUI("exampleData")
                     ) #demonstrates an example file
                   )
          ),
          #calculations tab
          tabPanel("Calculations", value = 3,
                   tabsetPanel(
                     id = "subPanel",
                     selected = 3.1,
                     tabPanel(HTML("2<sup>-(∆Cq)</sup>"), value = 3.1, #dcq tab
                              tabsetPanel(
                                id = "subCalc",
                                selected = 1,
                                tabPanel("All Data", value = 1, #dcq tab
                                         wrangleDataUI("wrangleDataModule"), # Display delta Cq data here, and filtered data (module_deltaCq.R)
                                         tags$br(),
                                         tags$br()),
                                #display biological replicate averages
                                tabPanel("Biological Replicate", value = 2,
                                         repDataUI("rep_data"),
                                )
                              )
                     ),
                     #delta delta cq tab
                     tabPanel(HTML("2<sup>-(∆∆Cq)</sup>"), value = 3.2,
                              tabsetPanel(
                                id = "subCalc2",
                                selected = 3,
                                tabPanel("All Data", value = 3,
                                         ddcqMain("ddcqModule")
                                ), #display processed ddcq data
                                tabPanel("Biological Replicate", value = 4,
                                         DDCQrepMain("ddcqRep"),
                                )
                              ),
                     ),
                   ),
          ),
          #statistics tab
          tabPanel("Statistics", value = 4,
                   statsMain("statsModule")
          ),
          tabPanel("Graphs", value = 5,
                   graphsMain("graphsModule")
          )
        )
      ) #main panel close bracket
    ) #sidebarLayout close bracket
  ) #fluidPage close bracket
  
  server <- function(input, output, session) {
    
    ## store reactive vals
    csv_data               <- reactiveVal(NULL)
    fileModule             <- reactiveVal(NULL)
    
    wrangled_data_module   <- reactiveVal(NULL)
    wrangled_data          <- reactiveVal(NULL)
    filter_condition       <- reactiveVal(NULL)
    
    rep_avg_data           <- reactiveVal(NULL)
    
    ddcq_data_module       <- reactiveVal(NULL)
    average_dcq            <- reactiveVal(NULL)
    selected_gene          <- reactiveVal(NULL)
    ddcq_repData           <- reactiveVal(NULL)
    
    stats_module           <- reactiveVal(NULL)
    
    graph_generated        <- reactiveVal(FALSE)
    
    # TAB 2 — FILE INPUT + VALIDATION
    observeEvent(input$tabselected == 2, {
      
      csv <- checkCSVfile("file") #insert csv file and check that it meets the required formatting
      fm  <- inputFileServer("file") # Generate dynamic text input fields for housekeeper genes
      
      downloadExampleData("file") #download example data file link
      inputDataServer("inputDataModule", csv) # Use the input data module to display inserted file in the UI
      
      csv_data(csv)
      fileModule(fm)
      
    }, once = TRUE, ignoreInit = TRUE)
    
    # TAB 3 — DCQ + DDCQ PIPELINE
    observeEvent(input$tabselected == 3, {
      
      req(csv_data(), fileModule())
      
      ## ---- DCQ wrangling ----
      wdm <- wrangleDataServer(
        "wrangleDataModule",
        fileModule()$save_btn,
        csv_data()$data,
        fileModule()$saved_variables
      ) #calculate mean housekeepers, delta Cq and fold change dcq
      
      # Display and calculate biological replicate average values for dcq. Save variables
      wrangled_data_module(wdm)
      wrangled_data(wdm$wrangled_data)
      filter_condition(wdm$filter_condition)
      
      #---- Biological replicate DCQ ----
      #perform biological replicate calculations
      DCQ_repData <- repDataServer(
        "rep_data",
        wrangled_data(),
        filter_condition()
      )
      #save dcq rep avg data table
      rep_avg_data(DCQ_repData$rep_avg_data)
      
      #---- DDCQ ----
      # select groups for ddcq and calculate ddcq for a gene
      # Display and calculate biological replicate average values for ddcq
      ddcq_mod <- ddcqServer("ddcqModule", wrangled_data())
      
      ddcq_data_module(ddcq_mod)
      average_dcq(ddcq_mod$average_dcq)
      selected_gene(ddcq_mod$extracted_gene)
      
      ddcq_rep_mod <- DDCQrepServer(
        "ddcqRep",
        average_dcq(),
        selected_gene()
      )
      
      ddcq_repData(ddcq_rep_mod$rep_avg_data_ddcq)
      
    }, once = TRUE, ignoreInit = TRUE)
    
    # TAB 4 — STATISTICS
    observeEvent(input$tabselected == 4, {
      
      req(
        ddcq_data_module(),
        wrangled_data(),
        average_dcq()
      )
      
      sm <- statsServer(
        "statsModule",
        values              = ddcq_data_module()$values,
        dcq_data            = wrangled_data(),
        ddcq_data           = average_dcq(),
        ddcq_selected_gene  = ddcq_data_module()$gene_for_download
      )
      
      stats_module(sm)
      
    }, once = TRUE, ignoreInit = TRUE)
    
    # TAB 5 — GRAPHING
    observeEvent(input$tabselected == 5, {
      
      req(
        ddcq_data_module(),
        wrangled_data(),
        average_dcq(),
        stats_module(),
        rep_avg_data(),
        ddcq_repData()
      )
      
      graphsServer(
        "graphsModule",
        tabselected              = reactive(input$tabselected),
        values                   = ddcq_data_module()$values,
        ddcq_repAvg              = ddcq_repData(),
        descriptivesTable        = stats_module()$descriptives_table,
        theme_Marnie             = theme_Marnie,
        wrangled_data            = wrangled_data(),
        ddcq_selected_gene       = ddcq_data_module()$extracted_gene,
        ddcq_data                = average_dcq(),
        select_dcq_or_ddcq_stats = stats_module()$selected_stat,
        stats_gene               = stats_module()$columnInput,
        shapiro_data_reactive    = stats_module()$filter_data_stats,
        graph_generated          = graph_generated,
        rep_avg_data             = rep_avg_data(),
        rep_avg_data_ddcq        = ddcq_repData(),
        comparisonResults        = stats_module()$comparisonResults,
        group_comparison         = stats_module()$group_comparison
      )
      
    }, once = TRUE, ignoreInit = TRUE)
    
  }
  shinyApp(ui, server, ...)
}