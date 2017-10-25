# tests for mranUrl()
if(interactive()) library(testthat)

context("mranURL")
test_that("sets snapshot correctly", {
  skip_on_cran()
  oldRepo <- getOption("repos")
  on.exit(options(repos = oldRepo))
  
  expect_equal(
    setSnapshot(),
    oldRepo
  )
  
  expect_message(
    setSnapshot("2017-04-01"),
    "Using CRAN mirror at"
  )
  
  expect_true(
    grepl("http.*://mran.microsoft.com/snapshot/2017-04-01", getOption("repos"))
  )
})