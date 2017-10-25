if(interactive()) library(testthat)

context("is.404")

test_that("is.404", {
  skip_if_offline()
  expect_true(is.404("http://mran.microsoft.com/snapshot/1972-01-01", warn = FALSE))
  expect_false(is.404("http://mran.microsoft.com/snapshot"))
  expect_false(is.404("http://mran.microsoft.com/snapshot/2015-05-01"))
})

test_that("is.404 works with https", {
  
  if(!httpsSupported()) skip("https not supported")
  expect_true(suppressWarnings(is.404("https://mran.microsoft.com/snapshot/1972-01-01")))
  expect_false(is.404("https://mran.microsoft.com/snapshot"))
  expect_false(is.404("https://mran.microsoft.com/snapshot/2015-05-01"))
  
})

test_that("is.404 gracefully deals with https URLs when https not supported", {
  
  with_mock(`checkpoint:::httpsSupported` = function(mran) FALSE, {
    # if(!httpsSupported()) skip("https not supported")
    expect_true(is.404("https://mran.microsoft.com/snapshot/1972-01-01", warn = FALSE))
    expect_true(is.404("https://mran.microsoft.com/snapshot", warn = FALSE))
    expect_true(is.404("https://mran.microsoft.com/snapshot/2015-05-01", warn = FALSE))
  })
  
})

test_that("is.404() deals with local file references", {
  localMRAN <- system.file("tests/localMRAN", package = "checkpoint")
  msg <- "Ensure you use the correct http://,  https:// or file:/// prefix."
  expect_error(is.404(localMRAN), msg)
  expect_false(is.404(paste0("file:///", localMRAN)))
})
