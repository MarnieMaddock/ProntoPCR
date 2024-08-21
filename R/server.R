
library(shiny)
source("module_inputData.R")
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
source("module_graphs.R")

source("utils_performComparisonTests.R")

# Define server logic 
function(input, output, session) {
  
  # #insert csv file and check that it meets the required formatting
  csv_data  <- checkCSVfile("file")
  downloadExampleData("file", dataset_path = "www/exampledata.csv")
  
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
  graphsServer("graphsModule", tabselected = reactive(input$tabselected), values = ddcq_data_module$values, ddcq_repAvg = ddcq_repData, descriptivesTable = descriptives_table, theme_Marnie, wrangled_data = wrangled_data, ddcq_selected_gene = ddcq_data_module$gene_for_download, ddcq_data = average_dcq, select_dcq_or_ddcq_stats = selected_stat,
               stats_gene = stats_gene, shapiro_data_reactive = filter_data_stats, graph_generated = graph_generated, rep_avg_data = rep_avg_data, rep_avg_data_ddcq = ddcq_repData, comparisonResults = comparisonResults, group_comparison = group_comparison)
}
