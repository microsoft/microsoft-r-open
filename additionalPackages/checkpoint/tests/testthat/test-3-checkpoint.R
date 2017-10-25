# tests for initialize
if(interactive()) library(testthat)

# Configure Travis for tests 
# https://github.com/RevolutionAnalytics/checkpoint/issues/139
Sys.setenv("R_TESTS" = "")

current.R <- local({ x = getRversion(); paste(x$major, x$minor, sep=".")})

test.start <- switch(current.R,
                     "3.1" = "2014-10-01",
                     "3.2" = "2015-05-01",
                     "3.3" = "2016-04-01",
                     "2016-04-01"
)

MRAN.default = test.start[1] # ensure only a single value

packages.to.test.base <- c("MASS", "chron", "checkpoint", "stats", "stats4", "compiler")
packages.to.test.base <- c("MASS", "chron", "checkpoint")
packages.to.test.knitr <- c("foreach")
checkpointLocation <- tempdir()
dir.create(file.path(checkpointLocation, ".checkpoint"), recursive = TRUE, showWarnings = FALSE)

#  ------------------------------------------------------------------------

test_checkpoint <- function(https = FALSE, snap_date){
  # snap_date <- test.start
  
  originalLibPaths <- .libPaths()
  
  url_prefix <- if(https) "https://" else "http://"
  # url_prefix <- "http://"
  # snap_date <- MRAN.default ### <<< use only for interactive testing
  
  packages.to.test = c(packages.to.test.base, packages.to.test.knitr) 
  project_root <- file.path(tempfile(), "checkpointtemp")
  dir.create(project_root, recursive = TRUE)
  
  cleanCheckpointFolder(snap_date, checkpointLocation = checkpointLocation)
  
  test_that(paste("checkpoint -", sub("//", "", url_prefix), "@", snap_date, "nothing to install"), {
    
    # finds correct MRAN URL"
    expect_equal(
      getSnapshotUrl(snap_date),
      paste0(url_prefix, "mran.microsoft.com/snapshot/", snap_date)
    )
    
    unCheckpoint()
    # prints message if no packages found"
    expect_message(
      checkpoint(snap_date, checkpointLocation = checkpointLocation, project = project_root),
      "No packages found to install"
    )
    
    unCheckpoint(originalLibPaths)
  })
  
  # expect_true(length(find.package("knitr", quiet = TRUE)) > 0)
  
  # Write dummy code file to project
  code = paste("library('", packages.to.test.base, "')", sep ="", collapse ="\n")
  cat(code, file = file.path(project_root, "code.R"))
  
  test_that(paste("checkpoint -", sub("//", "", url_prefix), "@", snap_date, "scan for base packages"), {
    expect_true(
      all(packages.to.test.base %in% scanForPackages(project_root, use.knitr = FALSE)$pkgs)
    )
  })
  
  # Write dummy knitr code file to project
  code = sprintf("```{r}\n%s\n```",
                 paste("library('", packages.to.test.knitr, "')", sep ="", collapse ="\n"))
  cat(code, file = file.path(project_root, "code.Rmd"))
  
  test_that(paste("checkpoint -", sub("//", "", url_prefix), "@", snap_date, "install all packages"), {
    if(!knitr.is.installed()) skip("knitr not available")
    
    expect_true(
      all(packages.to.test.knitr %in% scanForPackages(project_root, use.knitr = TRUE)$pkgs)
    )
    expect_true(
      all(packages.to.test %in% scanForPackages(project_root, use.knitr = TRUE)$pkgs)
    )
    
    # prints progress message
    unCheckpoint(originalLibPaths)
    expect_message(
      checkpoint(snap_date, project = project_root,
                 checkpointLocation = checkpointLocation, use.knitr = TRUE),
      "Installing packages used in this project"
    )
    
    # installs all packages correctly in local lib"
    pdbMRAN = available.packages(contriburl = contrib.url(repos = getSnapshotUrl(snap_date)))
    # pdbLocal = installed.packages(fields = "Date/Publication", noCache = TRUE)
    pdbLocal = installed.packages(noCache = TRUE)
    pkgNames = function(pdb)unname(pdb[, "Package"])
    base.packages <- pkgNames(utils::installed.packages(priority = "base", 
                                                        lib.loc = .Library,
                                                        noCache = TRUE))
    
    expected.packages <- setdiff(packages.to.test.base, c("checkpoint", base.packages))
    
    z <- expect_true(
      all(expected.packages %in% pkgNames(pdbLocal))
    )
    if(!z) message(pkgNames(pdbLocal))
    
    messageMissingPackages <- function(exp, avail){
      if(!all(exp %in% avail)) {
        msg <- paste(
          "\n",
          paste0("Expected:", paste(exp, collapse = ", ")),
          paste0("Actual  :", paste(avail, collapse = ", ")),
          paste0("Missing :", paste(setdiff(exp, avail), collapse = ", ")),
          "\n",
          sep = "\n")
        cat(msg)
      }
    }
    messageMissingPackages(expected.packages, pkgNames(pdbLocal))
    
    # re-installs packages when forceInstall=TRUE
    unCheckpoint(originalLibPaths)
    expect_message(
      checkpoint(snap_date,
                 checkpointLocation = checkpointLocation,
                 project = project_root, scanForPackages=TRUE, 
                 forceInstall = TRUE),
      "Removing packages to force re-install"
    )
    
    # writes log file in csv format
    logfile <- file.path(checkpointLocation, ".checkpoint/checkpoint_log.csv")
    expect_true(file.exists(logfile))
    expect_is(
      logdata <- read.csv(logfile, nrows = 5), 
      "data.frame"
    )
    expect_length(names(logdata), 4)
    
    # uses correct library location
    expect_equal(
      checkpointPath(snap_date, checkpointLocation, type = "lib"),
      normalizePath(.libPaths()[1], winslash = "/")
    )
    
    # uses correct MRAN url
    expect_equal(
      getOption("repos"),
      paste0(url_prefix, "mran.microsoft.com/snapshot/", snap_date)
    )
    
  })
  
  test_that(paste("checkpoint -", sub("//", "", url_prefix), "@", snap_date, "other tests"), {
    
    # does not display message whan scanForPackages=FALSE
    unCheckpoint(originalLibPaths)
    expect_false(
      isTRUE(
        shows_message("Scanning for packages used in this project",
                      checkpoint(snap_date, checkpointLocation = checkpointLocation,
                                 project = project_root, scanForPackages=FALSE)
        )
      ))
    
    # throws error when scanForPackages=FALSE and snapshotDate doesn't exist"
    unCheckpoint(originalLibPaths)
    expect_error(
      checkpoint("2015-01-01", checkpointLocation = checkpointLocation,
                 project = project_root, scanForPackages=FALSE),
      "Local snapshot location does not exist"
    )
    
    # stops when R.version doesn't match current version
    unCheckpoint(originalLibPaths)
    expect_error(
      checkpoint(snap_date, R.version = "2.15.0",
                 checkpointLocation = checkpointLocation,
                 project = project_root, scanForPackages=FALSE),
      "Specified R.version 2.15.0 does not match current R"
    )
  })
  
  test_that(paste("checkpoint -", sub("//", "", url_prefix), "@", snap_date, "cleanup"), {
    # cleanup
    cleanCheckpointFolder(snap_date, checkpointLocation = checkpointLocation)
    unCheckpoint(originalLibPaths)
    expect_identical(originalLibPaths, .libPaths())
  })
}


#  ------------------------------------------------------------------------

if(is_online()){
  if(TRUE){
    MRAN.dates <- getValidSnapshots()
    MRAN.sample <- sample(MRAN.dates, 2, replace = FALSE)
    
    initialUrl <- getOption("checkpoint.mranUrl")
    
    context("http ")
    options(checkpoint.mranUrl = "http://mran.microsoft.com/")
    test_checkpoint(http = FALSE, snap_date = MRAN.default)
    options(checkpoint.mranUrl = initialUrl)
    if(getRversion() >= "3.2.0" && httpsSupported()){
      context("https")
      options(checkpoint.mranUrl = "https://mran.microsoft.com/")
      test_checkpoint(http = TRUE, snap_date = MRAN.default)
      options(checkpoint.mranUrl = NULL)
    }
    
  } else {
    context("https")
    test_that("No tests run in offline mode", {
      skip("Offline - skipping all tests")
    })
    
  }
}
