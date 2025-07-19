
install.packages("testthat")
install.packages("shinytest2")

library(shinytest2)

test_app <- AppDriver$new("C:/Users/nab260/OneDrive - Newcastle University/Documents/IDF/app", name = "my_test", height = 800, width = 1200)

library(shinytest2)
library(testthat)

test_that("App works correctly", {
  app <- AppDriver$new(name = "shinytest2-demo", 
                       app_dir = "C:/Users/nab260/OneDrive - Newcastle University/Documents/IDF/app", 
                       height = 800, 
                       width = 1200)

  # Set inputs
  app$set_inputs(method = T)
  app$set_inputs(dur = "1h")

  # Check greeting output
  #app$expect_values(output = "greeting")

  # Check plot renders
  app$expect_screenshot(output = "idfPlot")

  app$stop()
})
