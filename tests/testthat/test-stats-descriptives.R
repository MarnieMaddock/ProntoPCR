source("helper-initialize.R") 

test_that("Descriptive stats function", {
  app <- initialize_app_with_housekeepers()
  on.exit(app$stop(), add = TRUE)
  
  # Simulate selecting the "Calculations" tab (value = 3)
  app$set_inputs(tabselected = "4")
  app$wait_for_idle()  # Give the app enough time to update

  app$set_inputs(`statsModule-statsData-sampleInput` = c("F_sample1", "M_sample1", "F_sample2", "M_sample2"))
  app$set_inputs(`statsModule-statsData-columnInput` = "fc_dcq_ISL1")
  app$set_inputs(`statsModule-descriptiveStats-descriptiveStats` = c("sample_size", "mean", "geo_mean", "median", "sd", "se", "ci", "variance", "minMax"))
  app$wait_for_idle()  # Give the app enough time to update

  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
})