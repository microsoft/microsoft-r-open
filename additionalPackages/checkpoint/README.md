# checkpoint - Install Packages from Snapshots on the Checkpoint Server for Reproducibility
[![Project Status: Active ? The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![Build Status](https://travis-ci.org/RevolutionAnalytics/checkpoint.svg?branch=dev)](https://travis-ci.org/RevolutionAnalytics/checkpoint)
[![codecov](https://codecov.io/gh/RevolutionAnalytics/checkpoint/branch/dev/graph/badge.svg)](https://codecov.io/gh/RevolutionAnalytics/checkpoint)
 
---
 

[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.0.0-6666ff.svg)](https://cran.r-project.org/)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/checkpoint)](https://cran.r-project.org/package=checkpoint)

[![packageversion](https://img.shields.io/badge/Package%20version-0.4.0-orange.svg?style=flat-square)](commits/master)
 
---
 
[![Last-changedate](https://img.shields.io/badge/last%20change-2017--04--11-yellowgreen.svg)](/commits/master)

## Overview

The goal of `checkpoint` is to solve the problem of package reproducibility in R. Specifically, `checkpoint` solve the problems that occur when you don't have the correct versions of R packages.  Since packages get updated on CRAN all the time, it can be difficult to recreate an environment where all your packages are consistent with some earlier state.

To solve this, `checkpoint` allows you to install packages from a specific snapshot date.  In other words, `checkpoint` makes it possible to install package versions from a specific date in the past, as if you had a CRAN time machine.


### Checkpoint Features

With the `checkpoint` package, you can easily:

* Write R scripts or projects using package versions from a specific point in time;
* Write R scripts that use older versions of packages, or packages that are no longer available on CRAN;
* Install packages (or package versions) visible only to a specific project, without affecting other R projects or R users on the same system;
* Manage multiple projects that use different package versions;
* Share R scripts with others that will automatically install the appropriate package versions;
* Write and share code R whose results can be reproduced, even if new (and possibly incompatible) package versions are released later.

## Using the checkpoint function

Using `checkpoint` is simple:

- The `checkpoint` package has only a single function, `checkpoint()` where you specify the snapshot date.
- Example: `checkpoint("2015-01-15")` instructs R to install and use only package versions that existed on January 15, 2015.

To write R code for reproducibility, simply begin your master R script as follows:

```R
library(checkpoint)
checkpoint("2015-01-15") ## or any date in YYYY-MM-DD format after 2014-09-17
```

Choose a snapshot date that includes the package versions you need for your script (or today's date, to get the latest versions). Any package version published since September 17, 2014 is available for use.

### Sharing your scripts for reproducibility

Sharing your R analysis reproducibly can be as easy as emailing a single R script. Begin your script with the following commands:


- Load the `checkpoint` package using `library(checkpoint)`
- Ensure you specify `checkpoint()` with your checkpoint date, e.g. `checkpoint("2014-10-01")`

Then send this script to your collaborators.  When they run this script on their machine, `checkpoint` will perform the same steps of installing the necessary packages, creating the `checkpoint` snapshot folder and producing the same results.


## How checkpoint works

When you create a checkpoint, the `checkpoint()` function performs the following:

- Creates a snapshot folder to install packages. This library folder is located at `~/.checkpoint`
- Scans your project folder for all packages used. Specifically, it searches for all instances of `library()` and `require()` in your code.
- Installs these packages from the MRAN snapshot into your snapshot folder using `install.packages()`
- Sets options for your CRAN mirror to point to a MRAN snapshot, i.e. modify `options(repos)`

This means the remainder of your script will run with the packages from a specific date.

### Where `checkpoint` finds historic package versions

To achieve reproducibility, once a day we create a complete snapshot of CRAN, on the "Managed R archived network" (MRAN) server.  At midnight (UTC) MRAN mirrors all of CRAN and saves a snapshot.  (MRAN has been storing daily snapshots since September 17, 2014.) This allows you to install packages from a snapshot date, thus "going back in time" to this date, by installing packages as they were at that snapshot date.


Together, the `checkpoint` package and the MRAN server act as a CRAN time machine. The `checkpoint()` function installs the packages to a local library exactly as they were at the specified point in time. Only those packages are available to your session, thereby avoiding any package updates that came later and may have altered your results. In this way, anyone using `checkpoint()` can ensure the reproducibility of your scripts or projects at any time.


## Resetting the checkpoint

To revert to your default CRAN mirror and access globally-installed packages, simply restart your R session. You can also use the experimental function `unCheckpoint()` - this resets your `.libPaths()`.



## Worked example

```r

# Create temporary project and set working directory

example_project <- paste0("~/checkpoint_example_project_", Sys.Date())

dir.create(example_project, recursive = TRUE)
oldwd <- setwd(example_project)


# Write dummy code file to project

cat("library(MASS)", "library(foreach)",
    sep="\n", 
    file="checkpoint_example_code.R")


# Create a checkpoint by specifying a snapshot date

library(checkpoint)
checkpoint("2014-10-01")

# Check that CRAN mirror is set to MRAN snapshot
getOption("repos")

# Check that library path is set to ~/.checkpoint
.libPaths()

# Check which packages are installed in checkpoint library
installed.packages()

# cleanup
unlink(example_project, recursive = TRUE)
setwd(oldwd)
```



## Installation

To install `checkpoint` directly from CRAN, use:

```r
install.packages("checkpoint")
library("checkpoint")
```

To install `checkpoint` directly from github, use the `devtools` package.  In your R session, try:

```r
install.packages("devtools")
devtools::install_github("RevolutionAnalytics/checkpoint")
library("checkpoint")
```


## Using knitr and rmarkdown with checkpoint

Although `checkpoint` will scan for dependencies in `.Rmd` files if `knitr` is installed, it does not automatically install the `knitr` or `rmarkdown` packages.

To build your `.Rmd` files, you will have to add a script in your project that explicitly loads all the packages required to build your `.Rmd` files.

A line like the following may be sufficient:

```r
library(rmarkdown)
```

This should automatically resolve dependencies on the packages `knitr`, `yaml` and `htmltools`

To build your `rmarkdown` file, use a call to `rmarkdown::render()`.  For example, to build a file called `example.Rmd`, use:

```r
rmarkdown::render("example.Rmd")
```



## More information

### Project website

https://github.com/RevolutionAnalytics/checkpoint/wiki

### Issues

Post an issue on the Issue tracker at https://github.com/RevolutionAnalytics/checkpoint/issues


### Checkpoint server

https://github.com/RevolutionAnalytics/checkpoint-server

### Made by

[Microsoft](https://mran.microsoft.com/)

## Code of conduct

This project has adopted the [Microsoft Open Source Code of Conduct](https://microsoft.github.io/codeofconduct). For more information see the [Code of Conduct FAQ](https://microsoft.github.io/codeofconduct/faq.md) or contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.

