# tests for access date
if(interactive()) library(testthat)

context("accessdate")
test_that("it detaches selected packages", {
  td <- make_fake_archive()
  x <- list.dirs(file.path(td, ".checkpoint"), full.names = FALSE, recursive = FALSE)
  
  
  # finds archive dates"
  exp <- c(
    "2099-03-01",
    "2099-01-02",
    "2099-04-01",
    "2099-02-01",
    "2099-06-01",
    "2099-02-01"
  )
  
  # "finds last access date"
  expect_equal(
    basename(getAccessDate(td)),
    exp
  )
  
  
  # finds archive dates
  exp <- c(
    "2099-01-01",
    "2099-01-02",
    "2099-01-03",
    "2099-01-04",
    "2099-01-05",
    "2099-01-06"
  )
  
  # "finds last access date"
  expect_equal(
    checkpointArchives(td),
    exp
  )
  
})

test_that("deletes archives", {
  td <- make_fake_archive()
  exp <- c(
    "2099-01-01",
    "2099-01-02",
    "2099-01-03",
    "2099-01-04",
    "2099-01-05",
    "2099-01-06"
  )
  # "finds deletes individual archive"
  expect_message(
    z <- checkpointRemove("2099-01-03", td),
    "successfully removed archive"
  )

  # "deletes range of archives"
  expect_message(
    z <- checkpointRemove("2099-01-03", td, allUntilSnapshot = TRUE),
    "successfully removed archive"
  )
  expect_equal(basename(z), exp[1:2])
  expect_message(
    z <- checkpointRemove("2099-01-03", td, allSinceSnapshot = TRUE),
    "successfully removed archive"
  )
  expect_equal(basename(z), exp[4:6])
})

test_that("deletes archives not used since a given access date", {
  td <- make_fake_archive()
  list.files(td, all.files = TRUE, recursive = TRUE)
  exp <- c(
    "2099-01-01",
    "2099-01-02",
    "2099-01-03",
    "2099-01-04",
    "2099-01-05",
    "2099-01-06"
  )
  # "deletes archives not used since"
  expect_message(
    z <- checkpointRemove("2099-05-01", td, notUsedSince = TRUE),
    "successfully removed archive"
  )
  expect_equal(basename(z), exp[c(1:4, 6)])
})
