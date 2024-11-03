source("helper-initialize.R") 

test_that("Test Group Comparisons", {
  app <- initialize_app_with_housekeepers()
  on.exit(app$stop(), add = TRUE)
  app$set_window_size(width = 1029, height = 640)
  # Simulate selecting the "Stats" tab (value = 4)
  app$set_inputs(tabselected = "4")
  app$wait_for_idle()  # Give the app enough time to update
  
  app$set_inputs(`statsModule-statsData-sampleInput` = c("F_sample1", "M_sample1", "F_sample2", "M_sample2"))
  app$set_inputs(`statsModule-statsData-columnInput` = "fc_dcq_ISL1")
  app$wait_for_idle()  # Give the app enough time to update
  #One-way ANOVA
  app$set_inputs(`statsModule-compTest-group_comparison` = "parametric")
  app$wait_for_idle()  # Give the app enough time to update
  
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #Kruskall wallis
  app$set_inputs(`statsModule-compTest-group_comparison` = "non_parametric")
  app$wait_for_idle()  # Give the app enough time to update

  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #welch test
  app$set_inputs(`statsModule-compTest-group_comparison` = "welch")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  app$set_inputs(`statsModule-statsData-sampleInput` = c("F_sample1", "M_sample1"))
  
  #t-test
  app$set_inputs(`statsModule-compTest-group_comparison` = "parametric")
  app$wait_for_idle()  # Give the app enough time to update
  
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #mann-whitney
  app$set_inputs(`statsModule-compTest-group_comparison` = "non_parametric")
  app$wait_for_idle()  # Give the app enough time to update
  
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #welch-test
  app$set_inputs(`statsModule-compTest-group_comparison` = "welch")
  app$wait_for_idle()  # Give the app enough time to update
  
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
})