# tests for checking if R files exist in project
if(interactive()) library(testthat)

context("any R files")

td <- make_fake_archive()

test_that("finds R files in checkpoint", {
  expect_false(
    anyRfiles(td)
  )
  writeLines("# Hello World", con = file.path(td, "hw.R"))
  expect_true(
    anyRfiles(td)
  )
})

unlink(file.path(td, "hw.R"))

test_that("finds no R files in fake checkpoint archive", {
  expect_false(
    anyRfiles(td)
  )
})

test_that("deals correctly with invalid project paths", {
  stub(validateProjectFolder, 
       "normalizePath",
       function(x, winslash, mustWork)"~/"
  )
  stub(validateProjectFolder,
       "readline", 
       function(x)"y"
  )
  expect_null(validateProjectFolder(td))
  
  stub(validateProjectFolder, 
       "normalizePath",
       function(x, winslash, mustWork)"~/"
  )
  stub(validateProjectFolder,
       "readline", 
       function(x)"n"
  )
  
  expect_error(validateProjectFolder(td), "Scanning stopped.")
  
})
