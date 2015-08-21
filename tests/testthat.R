library(testthat)
library(enzalyze)

test_check("enzalyze")

test_that("Input file must be a .csv"{
  
  expect_that(find_activity(uncal = "file.R", ...), throws_error())
  expect_that(find_activity(..., cal = "file.R", ...), throws_error())
})
