library(shiny)
library(shinyBS)
library(fontawesome)
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
library(rstatix)
library(multcompView)
library(Cairo)

source("about.R")
source("module_inputData.R")
source("module_exampleData.R")
source("module_deltaCq.R")
source("module_repavgDCQ.R")
source("module_deltadeltaCq.R")
source("module_repavgDDCQ.R")
source("module_download.R")
source("module_statsWorkflow.R")
source("module_MasterStats.R")
source("module_statsData.R")
source("module_descriptiveStats.R")
source("module_normalityStats.R")
source("utils_graphTheme.R")
source("module_leveneStats.R")
source("module_comparisonStats.R")
source("module_logStats.R")
source("module_graphs.R")


#Set the locale to ensure it handles UTF-8 encoding properly:
Sys.setlocale("LC_ALL", "en_US.UTF-8")


ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "pulse"), #theme
  tags$head(includeHTML("analytics.html")), #google analytics tracker
  includeCSS("www/style.css"), #custom css styles
  # Use div to place the logo
  div(id = "logo", tags$img(src = "dottori_lab_pentagon.svg")),
  div(id = "logo2", tags$img(src = "UOW.png")),
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
      conditionalPanel(condition = "input.tabselected == 4 && input.subStats == 1",
                       workflowSidebar("workflow"),
      ),
      conditionalPanel(condition = "input.tabselected == 4 && input.subStats == 2",
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
                 tabsetPanel(
                   id = "subStats",
                   selected = 1,
                   tabPanel("Flowchart", value = 1, # first subtab showing stats workflow recommendation
                            workflowMain("workflow"),
                   ),
                   tabPanel("Statistics", value = 2, #perform stats subtab
                            statsMain("statsModule"),
                   )
                 )
        ),
        tabPanel("Graphs", value = 5,
                 graphsMain("graphsModule")
        )
      )
    ) #main panel close bracket
  ) #sidebarLayout close bracket
) #fluidPage close bracket