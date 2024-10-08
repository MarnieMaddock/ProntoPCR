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
#' @export
ProntoPCR <-  function(...) {
  
  #Set the locale to ensure it handles UTF-8 encoding properly:
  #Sys.setlocale("LC_ALL", "en_US.UTF-8")
  
  
  ui <- fluidPage(
    theme = bslib::bs_theme(version = 4, bootswatch = "pulse"), #theme
    #tags$head(includeHTML(system.file("www", "analytics.html", package = "ProntoPCR"))),
    #tags$head(includeCSS("inst/www/style.css")),
    tags$head(includeCSS(system.file("www", "style.css", package = "ProntoPCR"))),
    
    # Use bslib::card_image to include images
    #div(id = "logo", bslib::card_image(file = "inst/www/dottori_lab_pentagon.svg", fill = FALSE, width = "70px")),
    #div(id = "logo2", bslib::card_image(file = "inst/www/UOW.png", fill = FALSE, width = "220px")),
    div(id = "logo", bslib::card_image(file = system.file("www", "dottori_lab_pentagon.svg", package = "ProntoPCR"), fill = FALSE, width = "70px")),
    div(id = "logo2", bslib::card_image(file = system.file("www", "UOW.png", package = "ProntoPCR"), fill = FALSE, width = "220px")),
    
    # Application title
    div(tags$h1("ProntoPCR v1.0.0", style = "margin-left: 65px;")),
    useShinyjs(), 
    #sidebar options
    sidebarLayout(
      sidebarPanel(
        style = "height: 85vh; overflow-y: auto;",
        conditionalPanel(condition = "input.tabselected==2 && input.subInput == 2.1",
                         inputFileUI("file")), #insert csv file and check that it meets the required formatting, enter housekeeper names and save them
        #Calculations tab: #Delta Cq tab
        conditionalPanel(condition = "input.tabselected == 3 && input.subPanel == 3.1",
                         wrangleDataSidebar("wrangleDataModule"),
        ),
        conditionalPanel(condition = "input.tabselected == 3 && input.subPanel == 3.2 && input.subCalc2 == 3",
                         ddcqSidebar("ddcqModule") #display ddcq data
        ),
        # Statistics tab
        # Suggested workflow tab
        conditionalPanel(condition = "input.tabselected == 4",
                         statsSidebar("statsModule"),
        ),
        conditionalPanel(condition = "input.tabselected == 5",
                         graphsSidebar("graphsModule")
        ),
      ), #sidebarPanel closing bracket
      
      mainPanel(
        tabsetPanel(
          type = "tabs", 
          id = "tabselected", 
          selected = 1, # Default tab selected is 1
          tabPanel("About", icon = icon("home", lib = "font-awesome"), textOutput("about"), value = 1,
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
                   statsMain("statsModule"),
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
                       regular = "inst/www/arial.ttf", 
                       italic = "inst/www/ariali.ttf", 
                       bold = "inst/www/arialbd.ttf", 
                       bolditalic = "inst/www/arialbi.ttf")
    sysfonts::font_add("Arial Bold", "inst/www/arialbd.ttf")
    sysfonts::font_add("Calibri", 
                       regular = "inst/www/calibri.ttf", 
                       italic = "inst/www/calibrii.ttf", 
                       bold = "inst/www/calibrib.ttf", 
                       bolditalic = "inst/www/calibriz.ttf")
    sysfonts::font_add("Times New Roman", 
                       regular = "inst/www/times.ttf", 
                       italic = "inst/www/timesi.ttf", 
                       bold = "inst/www/timesbd.ttf", 
                       bolditalic = "inst/www/timesbi.ttf")
    sysfonts::font_add("Georgia", 
                       regular = "inst/www/georgia.ttf", 
                       italic = "inst/www/georgiai.ttf", 
                       bold = "inst/www/georgiab.ttf", 
                       bolditalic = "inst/www/georgiaz.ttf")
    sysfonts::font_add("Comic Sans MS", 
                       regular = "inst/www/comic.ttf", 
                       italic = "inst/www/comici.ttf", 
                       bold = "inst/www/comicbd.ttf", 
                       bolditalic = "inst/www/comicz.ttf")
    sysfonts::font_add("Century Gothic", 
                       regular = "inst/www/GOTHIC.TTF", 
                       italic = "inst/www/GOTHICI.TTF", 
                       bold = "inst/www/GOTHICB.TTF", 
                       bolditalic = "inst/www/GOTHICBI.TTF")
    sysfonts::font_add("Tahoma", 
                       regular = "inst/www/tahoma.ttf",
                       bold = "inst/www/tahomabd.ttf")
    
    # #insert csv file and check that it meets the required formatting
    csv_data  <- checkCSVfile("file")
    downloadExampleData("file", dataset_path = "inst/www/exampledata.csv")
    # Generate dynamic text input fields for housekeeper genes
    fileModule <- inputFileServer("file")
    
    # Use the input data module to display inserted file in the UI
    inputDataServer("inputDataModule", csv_data)
    
    #calculate mean housekeepers, delta Cq and fold change dcq
    wrangled_data_module <- wrangleDataServer("wrangleDataModule", fileModule$save_btn, csv_data$data, fileModule$saved_variables)
    
    
    # Display and calculate biological replicate average values for dcq
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