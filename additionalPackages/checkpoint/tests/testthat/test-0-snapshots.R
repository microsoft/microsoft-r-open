if(interactive()) library(testthat)

context("snapshots")

test_that("stops if missing snapshotDate", {
  expect_error(
    checkpoint(), 
    "You have to specify a snapshotDate"
  )
})

test_that("stops if invalid snapshotDate format", {
  expect_error(
    checkpoint("2015/01/01"), 
    "snapshotDate must be a valid date using format YYYY-MM-DD"
  )
  expect_error(
    checkpoint("20150101"), 
    "snapshotDate must be a valid date using format YYYY-MM-DD"
  )
})

test_that("stops if snapshotDate doesn't exist on MRAN", {
  
  expect_error(
    checkpoint("2014-09-16"), 
    "Snapshots are only available after 2014-09-17"
  )
  expect_error(
    checkpoint(Sys.Date() + 1), 
    "snapshotDate can not be in the future!"
  )
})

test_that("set http/https correctly", {
  skip_on_cran()
  skip_if_offline()
  test_that("resolves to http/https based on R version number", {
    if(getRversion() >= "3.2.0"  && httpsSupported()){
      expect_warning(
        getSnapshotUrl("1972-01-01"), 
        "Unable to find snapshot on MRAN at https://mran.microsoft.com/snapshot/1972-01-01"
      )
    } else {
      expect_warning(
        getSnapshotUrl("1972-01-01"), 
        "Unable to find snapshot on MRAN at http://mran.microsoft.com/snapshot/1972-01-01"
      )
    }
    
    dd <- "2014-09-08"
    mm <- getSnapshotUrl(dd)
    expect_equal(paste0(mranUrl(), dd), mm)
    
    url <- mranUrl()
    if(getRversion() >= "3.2.0"  && httpsSupported()){
      expect_equal(url, "https://mran.microsoft.com/snapshot/")
    } else {
      expect_equal(url, "http://mran.microsoft.com/snapshot/")
    }
    
  })
})  

unCheckpoint()

