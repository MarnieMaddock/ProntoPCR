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

get_font_path <- function(file) {
  if (file.exists(paste0("inst/www/", file))) {
    return(paste0("inst/www/", file))
  } else {
    return(paste0("www/", file))
  }
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
  check_for_updates()
  
  # Display formal citation message in console
  message("\n==================================================================",
          "\n📖 If you use ProntoPCR for data analysis or in a journal article,",
          "\nplease cite the following reference:",
          "\n",
          "\n📌 Citation Here",
          "\n",
          "\n🙏 Thank you for supporting open-source research! 😊",
          "\n==================================================================\n")
  
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
    useShinyjs(), # Use shinyjs to hide and show elements
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
    # Enable automatic font rendering via showtext
    showtext::showtext_auto()
    # Add system fonts
    sysfonts::font_add("Arial", 
                       regular = get_font_path("arial.ttf"), 
                       italic = get_font_path("ariali.ttf"), 
                       bold = get_font_path("arialbd.ttf"), 
                       bolditalic = get_font_path("arialbi.ttf"))
    sysfonts::font_add("Arial Bold", get_font_path("arialbd.ttf"))
    sysfonts::font_add("Calibri", 
                       regular = get_font_path("calibri.ttf"), 
                       italic = get_font_path("calibrii.ttf"), 
                       bold = get_font_path("calibrib.ttf"), 
                       bolditalic = get_font_path("calibriz.ttf"))
    sysfonts::font_add("Times New Roman", 
                       regular = get_font_path("times.ttf"), 
                       italic = get_font_path("timesi.ttf"), 
                       bold = get_font_path("timesbd.ttf"), 
                       bolditalic = get_font_path("timesbi.ttf"))
    sysfonts::font_add("Georgia", 
                       regular = get_font_path("georgia.ttf"), 
                       italic = get_font_path("georgiai.ttf"), 
                       bold = get_font_path("georgiab.ttf"), 
                       bolditalic = get_font_path("georgiaz.ttf"))
    sysfonts::font_add("Comic Sans MS", 
                       regular = get_font_path("comic.ttf"), 
                       italic = get_font_path("comici.ttf"), 
                       bold = get_font_path("comicbd.ttf"), 
                       bolditalic = get_font_path("comicz.ttf"))
    sysfonts::font_add("Century Gothic", 
                       regular = get_font_path("GOTHIC.TTF"), 
                       italic = get_font_path("GOTHICI.TTF"), 
                       bold = get_font_path("GOTHICB.TTF"), 
                       bolditalic = get_font_path("GOTHICBI.TTF"))
    sysfonts::font_add("Tahoma", 
                       regular = get_font_path("tahoma.ttf"),
                       bold = get_font_path("tahomabd.ttf"))
    
    # #insert csv file and check that it meets the required formatting
    csv_data  <- checkCSVfile("file")
    downloadExampleData("file") #download example data file link
    # Generate dynamic text input fields for housekeeper genes
    fileModule <- inputFileServer("file")
    
    # Use the input data module to display inserted file in the UI
    inputDataServer("inputDataModule", csv_data)
    
    #calculate mean housekeepers, delta Cq and fold change dcq
    wrangled_data_module <- wrangleDataServer("wrangleDataModule", fileModule$save_btn, csv_data$data, fileModule$saved_variables)
    
    # Display and calculate biological replicate average values for dcq. Save variables
    wrangled_data <- wrangled_data_module$wrangled_data
    filter_condition <- wrangled_data_module$filter_condition
    
    #perform biological replicate calculations
    DCQ_repData <- repDataServer("rep_data", wrangled_data, filter_condition)
    #save dcq rep avg data table
    rep_avg_data <- DCQ_repData$rep_avg_data
    
    # select groups for ddcq and calculate ddcq for a gene
    # Display and calculate biological replicate average values for ddcq
    ddcq_data_module <- ddcqServer("ddcqModule", wrangled_data)
    average_dcq <- ddcq_data_module$average_dcq
    selected_gene <- ddcq_data_module$extracted_gene
    ddcq_rep_module <- DDCQrepServer("ddcqRep", average_dcq, selected_gene)
    ddcq_repData <- ddcq_rep_module$rep_avg_data_ddcq
    
    #statistics
    stats <- statsServer("statsModule", values = ddcq_data_module$values, dcq_data = wrangled_data, ddcq_data = average_dcq, ddcq_selected_gene = ddcq_data_module$gene_for_download)
    selected_stat <- stats$selected_stat
    stats_gene <- stats$columnInput
    filter_data_stats <- stats$filter_data_stats
    group_comparison <- stats$group_comparison
    comparisonResults <- reactive({
      stats$comparisonResults()
    })
    descriptives_table <- stats$descriptives_table
    
    # Graphing
    graph_generated <- reactiveVal(FALSE)
    graphsServer("graphsModule", tabselected = reactive(input$tabselected), values = ddcq_data_module$values, ddcq_repAvg = ddcq_repData, descriptivesTable = descriptives_table, theme_Marnie, wrangled_data = wrangled_data, ddcq_selected_gene = ddcq_data_module$extracted_gene, ddcq_data = average_dcq, select_dcq_or_ddcq_stats = selected_stat,
                 stats_gene = stats_gene, shapiro_data_reactive = filter_data_stats, graph_generated = graph_generated, rep_avg_data = rep_avg_data, rep_avg_data_ddcq = ddcq_repData, comparisonResults = comparisonResults, group_comparison = group_comparison)
    
  }
  shinyApp(ui, server, ...)
}