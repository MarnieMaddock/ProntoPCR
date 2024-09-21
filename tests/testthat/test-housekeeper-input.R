test_that("Housekeeper gene inputs are processed correctly", {
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
  
  # Set the number of housekeeper genes
  app$set_inputs(`file-housekeepers` = 3)
  app$wait_for_idle()
  
  # Scroll to the dynamic inputs
  app$run_js("document.getElementById('file-group1').scrollIntoView();")
  
  # Wait for the dynamic inputs to become available
  app$wait_for_js(
    script = "document.getElementById('file-group1') !== null",
    timeout = 10000
  )

  # Check if the element exists
  element_exists <- app$get_js("document.getElementById('file-group1') !== null")
  print(element_exists)  # Should print TRUE if the element exists
  
  # Input housekeeper gene names
  housekeeper_names <- c("PPIA", "B2M", "GAPDH")
  for (i in 1:3) {
    input_id <- paste0("file-group", i)
    app$set_inputs(!!input_id := housekeeper_names[i])
  }
  app$wait_for_idle()
  
  # Click the "Save Housekeeper Names" button
  app$click("file-save_btn")
  app$wait_for_idle()
  Sys.sleep(1)  # Wait for 1 second
  
  # Retrieve and verify the output text
  text1_output <- app$get_value(output = "file-text1")
  print(paste("text1_output:", text1_output))
  
  # Check that the output contains the correct number of housekeepers
  expect_true(grepl("You have 3 housekeeper genes", text1_output))
  
  # Check that all housekeeper names are present
  for (name in housekeeper_names) {
    expect_true(grepl(name, text1_output))
  }
  
  # Optionally, take a screenshot
  app$expect_screenshot()
})