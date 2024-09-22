source("helper-initialize.R") 

test_that("Housekeeper gene inputs are processed correctly", {
  app <- initialize_app_with_housekeepers()
  on.exit(app$stop(), add = TRUE)
  
  # Verify the output text
  text1_output <- app$get_value(output = "file-text1")
  
  # Check that the output contains the correct number of housekeepers
  expect_true(grepl("You have 3 housekeeper genes", text1_output))
  
  # Check that all housekeeper names are present
  housekeeper_names <- c("PPIA", "B2M", "GAPDH")
  for (name in housekeeper_names) {
    expect_true(grepl(name, text1_output))
  }
  
  # Optionally, take a screenshot
  app$expect_screenshot()
})