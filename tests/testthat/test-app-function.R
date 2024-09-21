

test_that("ProntoPCR app initial values are consistent", {
  
  # Call the function to get the Shiny app object
  shiny_app <- ProntoPCR()
  
  # Initialize AppDriver with the app object
  app <- shinytest2::AppDriver$new(shiny_app, name = "pronto_pcr")
  
  # Check that the initial values of the app are as expected
  app$expect_values()
})


