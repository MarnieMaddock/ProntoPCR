source("helper-initialize.R") 

test_that("Test Log2 and Log10 transformation", {
  app <- initialize_app_with_housekeepers()
  on.exit(app$stop(), add = TRUE)
  app$set_window_size(width = 1029, height = 640)
  # Simulate selecting the "Stats" tab (value = 4)
  app$set_inputs(tabselected = "4")
  app$wait_for_idle()  # Give the app enough time to update
  
  app$set_inputs(`statsModule-statsData-sampleInput` = c("F_sample1", "M_sample1", "F_sample2", "M_sample2"))
  app$set_inputs(`statsModule-statsData-columnInput` = "fc_dcq_ISL1")
  app$wait_for_idle()  # Give the app enough time to update
  app$set_inputs(`statsModule-descriptiveStats-descriptiveStats` = c("sample_size", "mean", "geo_mean", "median", "sd", "se", "ci", "variance", "minMax"))
  app$set_inputs(`statsModule-normalityStats-normality_test` = c("shapiro", "qqplot", "density"))
  app$set_inputs(`statsModule-leveneStats-variance` = TRUE)
  app$set_inputs(`statsModule-compTest-group_comparison` = "parametric")
  app$wait_for_idle()  # Give the app enough time to update
  # Update output value
  
  #test Log2
  app$set_inputs(`statsModule-logStats-log_transform` = "Log2")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #test log10
  app$set_inputs(`statsModule-logStats-log_transform` = "Log10")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
})