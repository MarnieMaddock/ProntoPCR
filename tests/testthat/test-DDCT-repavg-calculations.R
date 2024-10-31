source("helper-initialize.R") 

test_that("DDCT rep data initializes correctly", {
  app <- initialize_app_with_housekeepers()
  on.exit(app$stop(), add = TRUE)
  
  # Simulate selecting the "Calculations" tab (value = 3)
  app$set_inputs(tabselected = "3")
  app$wait_for_idle()  # Give the app enough time to update
  
  # Simulate selecting the "Calculations" tab DDCT 
  app$set_inputs(subPanel = "3.2")
  app$wait_for_idle()  # Give the app enough time to update
  
  #all data
  app$set_inputs(subCalc2 = "4")
  app$wait_for_idle()  # Give the app enough time to update
  
  # Scroll to the dynamic inputs
  app$run_js("document.getElementById('ddcqModule-select_gene').scrollIntoView();")
  
  # Wait for the dynamic inputs to become available
  app$wait_for_js("document.getElementById('ddcqModule-select_control') !== null", timeout = 10000)
  app$wait_for_js("document.getElementById('ddcqModule-select_samples') !== null", timeout = 10000)
  app$wait_for_js("document.getElementById('ddcqModule-select_gene') !== null", timeout = 10000)

  # Set the control group
  app$set_inputs(`ddcqModule-select_control` = "F_sample1")
  
  # Set the sample groups (multiple selections)
  sample_groups <- c("M_sample1", "F_sample2", "M_sample2", "F_sample3", "M_sample3")
  app$set_inputs(`ddcqModule-select_samples` = sample_groups)
  
  # Set the gene column
  app$set_inputs(`ddcqModule-select_gene` = "dcq_TP53")
  
  app$wait_for_idle()
  
  # Click the "Save ΔΔCq Data" button
  app$click("ddcqModule-save_ddcq_data")
  app$wait_for_idle()

  #rep data
  app$set_inputs(subCalc = "4")
  app$wait_for_idle()  # Give the app enough time to update

  # Check that certain values are calculated correctly
  app$expect_values(output = "ddcqRep-rep_avg_table_ddcq")
  
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
})