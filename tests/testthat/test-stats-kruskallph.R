source("helper-initialize.R") 

test_that("Kruskall post-hocs", {
  app <- initialize_app_with_housekeepers()
  on.exit(app$stop(), add = TRUE)
  
  # Simulate selecting the "Stats" tab (value = 4)
  app$set_inputs(tabselected = "4")
  app$wait_for_idle()  # Give the app enough time to update
  
  app$set_inputs(`statsModule-statsData-sampleInput` = c("F_sample1", "M_sample1", "F_sample2", "M_sample2"))
  app$set_inputs(`statsModule-statsData-columnInput` = "fc_dcq_ISL1")
  app$wait_for_idle()  # Give the app enough time to update
  
  app$set_inputs(`statsModule-compTest-group_comparison` = "non_parametric")
  #bonferonni correction
  app$wait_for_idle()
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #sidak correction
  app$set_inputs(`statsModule-compTest-correctionMethod` = "sidak")
  app$wait_for_idle()
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #holm correction
  app$set_inputs(`statsModule-compTest-correctionMethod` = "holm")
  app$wait_for_idle()
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #holm sidak correction
  app$set_inputs(`statsModule-compTest-correctionMethod` = "hs")
  app$wait_for_idle()
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #Benjamini-Hochberg correction
  app$set_inputs(`statsModule-compTest-correctionMethod` = "bh")
  app$wait_for_idle()
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #hochberg correction
  app$set_inputs(`statsModule-compTest-correctionMethod` = "hochberg")
  app$wait_for_idle()
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
})