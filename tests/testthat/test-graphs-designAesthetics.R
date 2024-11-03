source("helper-initialize.R") 

test_that("Graph Design aesthetics", {
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
  
  #grouped scatter plot
  app$set_inputs(`graphsModule-plot_type` = "dot_group")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  # #legend labels
  app$set_inputs(`graphsModule-legend_title` = "Condition test works")
  app$set_inputs(`graphsModule-legend_position` = "top")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))

  
  #column graph
  app$set_inputs(`graphsModule-plot_type` = "column")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #test some colours
  app$set_inputs(`graphsModule-color_scheme_select` = "colourblind1")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  app$set_inputs(`graphsModule-color_scheme_select` = "grays")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  app$set_inputs(`graphsModule-color_scheme_select` = "blaze")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  app$set_inputs(`graphsModule-color_scheme_select` = "vibrant")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  app$set_inputs(`graphsModule-color_scheme_select` = "marnie")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #error bars
  app$set_inputs(`graphsModule-error_type` = "sd")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))

  app$set_inputs(`graphsModule-error_type` = "se")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))

  app$set_inputs(`graphsModule-error_type` = "ci")
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))

  #column graph error bar options
  app$set_inputs(`graphsModule-errorbar_width` = 0.65)
  app$set_inputs(`graphsModule-errorbar_thickness` = 3.25)
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))

  #dot plot error bar options
  app$set_inputs(`graphsModule-plot_type` = "dot_group")
  app$set_inputs(`graphsModule-error_bar_width` = 0.35)
  app$set_inputs(`graphsModule-average_line_width` = 0.26)
  app$set_inputs(`graphsModule-error_bar_thickness` = 1.85)
  app$set_inputs(`graphsModule-average_line_thickness` = 1.85)
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #dot plot
  app$set_inputs(`graphsModule-plot_type` = "dot")
  app$set_inputs(`graphsModule-match_colour_error` = TRUE)
  app$set_inputs(`graphsModule-match_colour_avg` = TRUE)
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #Start Y axis at 0
  app$set_inputs(`graphsModule-start_at_zero` = FALSE)
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  #point design
  app$set_inputs(`graphsModule-point_size` = 6)
  app$set_inputs(`graphsModule-change_shapes` = TRUE)
  app$set_inputs(`graphsModule-stroke_thickness` = 2.1)
  app$set_inputs(`graphsModule-jitter_amount` = 0.4)
  app$set_inputs(`graphsModule-seed_input` = 125)
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
  app$set_inputs(`graphsModule-width` = 6)
  app$set_inputs(`graphsModule-height` = 4)
  filename <- "Graph_IL6_test.svg"  # Static name for testing
  app$expect_download("graphsModule-downloadGraph", name = filename)
  app$wait_for_idle()  # Give the app enough time to update
  # Take a full-page screenshot of basic graph
  app$expect_screenshot(screenshot_args = list(
    cliprect = NULL,
    selector = "html"
  ))
  
})