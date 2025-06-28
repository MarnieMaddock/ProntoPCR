# File: tests/testthat/helper-initialize.R
options(
  shinytest2.headless = TRUE,
  shinytest2.viewport_width = NULL,
  shinytest2.viewport_height = NULL,
  shinytest2.view = FALSE
)


initialize_app <- function() {
  # Initialize the Shiny app
  app <- shinytest2::AppDriver$new(ProntoPCR(), name = "pronto_pcr", 
                                   variant = shinytest2::platform_variant(),
                                   height = 1000,
                                   width = 1600,
                                   seed = 123,
                                   shiny_args = list(launch.browser = TRUE))
  
  
  # Simulate selecting the "Input Data" tab (value = 2)
  app$set_inputs(tabselected = "2")
  app$wait_for_idle()  # Give the app enough time to update
  
  # Simulate selecting the "Data" subtab (value = 2.1)
  app$set_inputs(subInput = "2.1")
  app$wait_for_idle()  # Give the app enough time to update
  
  # Upload a CSV file
  test_csv_path <- system.file("testdata", "exampledata.csv", package = "ProntoPCR")
  
  if (!file.exists(test_csv_path)) {
    stop("Test CSV file does not exist at the specified path.")
  }
  
  app$upload_file(`file-file` = test_csv_path)
  app$wait_for_idle()
  
  return(app)
}

set_housekeeper_genes <- function(app, housekeeper_names) {
  # Set the number of housekeeper genes
  app$set_inputs(`file-housekeepers` = length(housekeeper_names))
  app$wait_for_idle()
  
  # Scroll to the dynamic inputs
  app$run_js("document.getElementById('file-group1').scrollIntoView();")
  
  # Wait for the dynamic inputs to become available
  app$wait_for_js(
    script = "document.getElementById('file-group1') !== null",
    timeout = 10000
  )
  
  # Input housekeeper gene names
  for (i in seq_along(housekeeper_names)) {
    input_id <- paste0("file-group", i)
    app$set_inputs(!!input_id := housekeeper_names[i])
  }
  app$wait_for_idle()
  
  # Click the "Save Housekeeper Names" button
  app$click("file-save_btn")
  app$wait_for_idle()
}

initialize_app_with_housekeepers <- function(housekeeper_names = c("PPIA", "B2M", "GAPDH")) {
  app <- initialize_app()
  set_housekeeper_genes(app, housekeeper_names)
  return(app)
}