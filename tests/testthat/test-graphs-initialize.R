source("helper-initialize.R") 

test_that("Graphs initialize", {
  app <- initialize_app_with_housekeepers()
  on.exit(app$stop(), add = TRUE)
  app$set_window_size(width = 1029, height = 640)
  #set up stats tab so that graphs will gnerate with no errors
  # Simulate selecting the "Stats" tab (value = 4)
  app$set_inputs(tabselected = "4")
  app$wait_for_idle()  # Give the app enough time to update
  
  app$set_inputs(`statsModule-statsData-sampleInput` = c("F_sample1", "M_sample1", "F_sample2", "M_sample2"))
  app$wait_for_idle()  # Give the app enough time to update
  
  app$set_inputs(`statsModule-compTest-group_comparison` = "parametric")
  #tukey auto-selected
  app$wait_for_idle()  # Give the app enough time to update
  
  #select graphs tab, samples and gene
  app$set_inputs(tabselected = "5")
  app$wait_for_idle()  # Give the app enough time to update
  app$set_inputs(`graphsModule-selected_condition` = c("F_sample1", "M_sample1", "F_sample2", "M_sample2"))
  app$wait_for_idle()  # Give the app enough time to update
  
  #place x axis positions
  app$set_inputs(`graphsModule-x_axis_positions` = "F_sample1,M_sample1,F_sample2,M_sample2")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
})