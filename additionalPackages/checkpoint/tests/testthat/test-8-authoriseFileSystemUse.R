# tests for checking file system authorization
if(interactive()) library(testthat)
library(mockery)

context("file system authorization")
td <- file.path(tempdir(), "checkpoint_not_auth")
unlink(td, recursive = TRUE)

test_that("stops without authorization in interactive mode", {
  stub(authorizeFileSystemUse, 
       "readline", 
       function(prompt)"n"
  )
  expect_error(
    authorizeFileSystemUse(td, interactive=TRUE),
    "Cannot proceed without access to checkpoint directory"
  )
})

test_that("continues with authorization in interactive mode", {
  stub(authorizeFileSystemUse,
       "readline", 
       function(prompt)"y"
  )
  expect_null(
    authorizeFileSystemUse(td, interactive=TRUE)    
  )
})

test_that("stops if folder doesn't exist in batch mode", {
  expect_error(
    authorizeFileSystemUse(td, interactive=FALSE),
    "The .checkpoint folder does not exist. Please try again after creating the folder at"
  )
})

