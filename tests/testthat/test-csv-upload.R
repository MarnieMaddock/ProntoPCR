# File: tests/testthat/test-csv-upload.R

test_that("CSV file upload and calculations work as expected", {
  # Initialize the Shiny app
  app <- shinytest2::AppDriver$new(ProntoPCR(), name = "pronto_pcr", variant = shinytest2::platform_variant())    # Adjust width as needed)
  on.exit(app$stop(), add = TRUE)
  
  # Simulate selecting the "Input Data" tab (value = 2)
  app$set_inputs(tabselected = "2")
  app$wait_for_idle()  # Give the app enough time to update
  
  # Simulate selecting the "Data" subtab (value = 2.1)
  app$set_inputs(subInput = "2.1")
  app$wait_for_idle()  # Give the app enough time to update
  
  # Upload a CSV file (assume you have a sample CSV in the tests folder)
  test_csv_path <- system.file("testdata", "exampledata.csv", package = "ProntoPCR")
  # Check if the file exists
  if (!file.exists(test_csv_path)) {
    stop("Test CSV file does not exist at the specified path.")
  }
  
  #app$upload_file(input = "file-file", file = test_csv_path)
  app$upload_file(`file-file` = test_csv_path)
  app$wait_for_idle()
  
  # Check that certain values are calculated correctly
  app$expect_values(output = "inputDataModule-table")
  
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
})
