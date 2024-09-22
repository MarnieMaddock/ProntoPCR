source("helper-initialize.R") 

# File: tests/testthat/test-csv-upload.R

test_that("CSV file upload and calculations work as expected", {
  # Initialize the app using the helper function
  app <- initialize_app()
  on.exit(app$stop(), add = TRUE)
  
  # Check that certain values are calculated correctly
  app$expect_values(output = "inputDataModule-table")
  
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
})
