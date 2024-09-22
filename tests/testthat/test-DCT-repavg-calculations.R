source("helper-initialize.R") 

test_that("Rep average DCT initializes correctly", {
  app <- initialize_app_with_housekeepers()
  on.exit(app$stop(), add = TRUE)
  
  # Simulate selecting the "Calculations" tab (value = 3)
  app$set_inputs(tabselected = "3")
  app$wait_for_idle()  # Give the app enough time to update
  
  # Simulate selecting the "Calculations" tab DCT 
  app$set_inputs(subPanel = "3.1")
  app$wait_for_idle()  # Give the app enough time to update
  
  #all data
  app$set_inputs(subCalc = "2")
  app$wait_for_idle()  # Give the app enough time to update
  
  
  # Check that certain values are calculated correctly
  app$expect_values(output = "rep_data-rep_avg_table")
  
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  
  # Optionally, take a screenshot
  app$expect_screenshot()
})