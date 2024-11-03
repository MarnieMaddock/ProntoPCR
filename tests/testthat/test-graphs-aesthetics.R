source("helper-initialize.R") 

test_that("Graphs aesthetics are customisable", {
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
  #test that the x axis labels can be chnaged and rotated
  app$set_inputs(`graphsModule-label_F_sample1` = "Female")
  app$set_inputs(`graphsModule-label_M_sample1` = "Male")
  app$set_inputs(`graphsModule-rotate_labels` = TRUE)
  
  app$wait_for_idle()  # Give the app enough time to update
  
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  
  #y axis and x axis labels
  app$set_inputs(`graphsModule-y_label` = "Y axis works")
  app$set_inputs(`graphsModule-x_label` = "x axis works")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #fonts
  app$set_inputs(`graphsModule-font_selector` = "Arial Bold")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  app$set_inputs(`graphsModule-font_selector` = "Calibri")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  app$set_inputs(`graphsModule-font_selector` = "Times New Roman")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  app$set_inputs(`graphsModule-font_selector` = "Georgia")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  app$set_inputs(`graphsModule-font_selector` = "Comic Sans MS")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  app$set_inputs(`graphsModule-font_selector` = "Century Gothic")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  app$set_inputs(`graphsModule-font_selector` = "Tahoma")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #font size
  app$set_inputs(`graphsModule-x_axis_title_font_size` = 25)
  app$set_inputs(`graphsModule-y_axis_title_font_size` = 20)
  app$set_inputs(`graphsModule-x_axis_label_font_size` = 7)
  app$set_inputs(`graphsModule-y_axis_label_font_size` = 8)
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  
})