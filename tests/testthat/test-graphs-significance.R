source("helper-initialize.R") 

test_that("Graph Significance", {
  app <- initialize_app_with_housekeepers()
  on.exit(app$stop(), add = TRUE)
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
  
  #significance
  app$set_inputs(`graphsModule-add_significance` = "asterix")
  app$wait_for_idle()  # Give the app enough time to update
  app$set_inputs(`graphsModule-sigSize` = 7)
  app$set_inputs(`graphsModule-bracketSize` = 1)
  app$set_inputs(`graphsModule-stepIncrease` = 0.2)
  app$set_inputs(`graphsModule-tipLength` = 0.04)
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  app$set_inputs(`graphsModule-add_significance` = "pval")
  app$set_inputs(`graphsModule-pValuePrefix` = "P = ")
  app$set_inputs(`graphsModule-pValueDecimals` = 4)
  app$set_inputs(`graphsModule-remove0` = TRUE)
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  app$set_inputs(`graphsModule-add_significance` = "cld")
  app$set_inputs(`graphsModule-sigSize` = 8)
  app$set_inputs(`graphsModule-stepIncrease` = 0)
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
})