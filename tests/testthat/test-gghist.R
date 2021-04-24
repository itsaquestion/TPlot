context("test-gghist")


test_that("gghist", {
  # 
  # library(xts)
  # library(checkmate)
  # #library(purrr)
  # library(ggplot2)
  # library(directlabels)
  # library(egg)
  # library(TTR)
  
 
  
  x = rnorm(1000)
  
  #expect_that(,not(gives_warning()))
  
  expect_warning(gghist(x,density = T), regexp = NA)
  
  
})




















