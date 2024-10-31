source("helper-initialize.R") 

test_that("ANOVA post-hocs", {
  app <- initialize_app_with_housekeepers()
  on.exit(app$stop(), add = TRUE)
  
  # Simulate selecting the "Stats" tab (value = 4)
  app$set_inputs(tabselected = "4")
  app$wait_for_idle()  # Give the app enough time to update
  
  app$set_inputs(`statsModule-statsData-sampleInput` = c("F_sample1", "M_sample1", "F_sample2", "M_sample2"))
  app$set_inputs(`statsModule-statsData-columnInput` = "fc_dcq_ISL1")
  app$wait_for_idle()  # Give the app enough time to update
  
  app$set_inputs(`statsModule-compTest-group_comparison` = "parametric")
  #tukey auto-selected
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #bonferonni post hoc
  app$set_inputs(`statsModule-compTest-postHocTest` = "bonferroni")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #holm post hoc
  app$set_inputs(`statsModule-compTest-postHocTest` = "holm")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  app$set_inputs(`statsModule-compTest-postHocTest` = "bh")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  #Scheffe test
  app$set_inputs(`statsModule-compTest-postHocTest` = "scheffe")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
})