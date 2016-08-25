# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

"isPackageLoadable" <- function (pkg, lib.loc=NULL, quiet=TRUE) 
{    
  RevoIOQ:::testPackageLoadability(pkg = pkg, lib.loc = lib.loc, quiet = quiet)
}

# define list of base and recommended packages for rollback 
base <- c("base", "datasets", "graphics", "grDevices", "grid", "methods", "splines", "stats", "stats4", "tools", "utils")
recommended <- c("boot", "class", "cluster", "codetools", "foreign", "KernSmooth", "lattice", "MASS", "mgcv", "nlme", "nnet", "rpart", "spatial", "survival")

# add tcltk to list of base packages if not on LINUX
if (!length(grep("linux",version$os)))
{
    base <- c(base, "tcltk")
}

# store current package tests
session <- RevoIOQ:::saveRUnitSession(packages=c(base,recommended))

# run tests on base packages
"test.base.package.base.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "base"))
"test.base.package.datasets.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "datasets"))
"test.base.package.graphics.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "graphics"))
"test.base.package.grDevices.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "grDevices"))
"test.base.package.grid.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "grid"))
"test.base.package.methods.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "methods"))
"test.base.package.splines.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "splines"))
"test.base.package.stats.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "stats"))
"test.base.package.stats4.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "stats4"))
"test.base.package.tools.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "tools"))
"test.base.package.utils.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "utils"))
"test.base.package.tcltk.loadability" <- function()
{
  # check tcltk if not on LINUX
  if (!length(grep("linux",version$os))) checkTrue("isPackageLoadable"(pkg = "tcltk")) else TRUE
}

# run recommended package tests
"test.recommended.package.boot.loadability" <- function() checkTrue("isPackageLoadable"(pkg ="boot"))
"test.recommended.package.class.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "class"))
"test.recommended.package.cluster.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "cluster"))
"test.recommended.package.codetools.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "codetools"))
"test.recommended.package.foreign.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "foreign"))
"test.recommended.package.KernSmooth.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "KernSmooth"))
"test.recommended.package.lattice.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "lattice"))
"test.recommended.package.MASS.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "MASS"))
"test.recommended.package.mgcv.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "mgcv"))
"test.recommended.package.nlme.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "nlme"))
"test.recommended.package.nnet.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "nnet"))
"test.recommended.package.rpart.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "rpart"))
"test.recommended.package.spatial.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "spatial"))
"test.recommended.package.survival.loadability" <- function() checkTrue("isPackageLoadable"(pkg = "survival"))         

# restore stored session state
"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}
