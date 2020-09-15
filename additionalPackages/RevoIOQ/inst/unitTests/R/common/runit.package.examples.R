"runPackageExamples" <- function (pkg, bundle, outDir = file.path(tempdir(), "runPackageExamplesDir"), types = c("examples", "tests", "vignettes")) 
{
   if (!(file.exists(outDir) && file.info(outDir)$isdir))
   {
     dir.create(outDir, showWarnings=FALSE, recursive=TRUE)
     on.exit(unlink(outDir))
   }
   
   if (compareVersion(paste(R.version$major, R.version$minor, sep="."),"2.9.2") >= 0)
       tools::testInstalledPackage(pkg = pkg, lib.loc =.Library, outDir = outDir, types = types) == 0L
   else TRUE
}

# define list of base and recommended packages for rollback
priority <- installed.packages()[,"Priority"]
baserec <- names(priority)[priority %in% c("base","recommended")]

# store current package tests
session <- RevoIOQ:::saveRUnitSession(packages=baserec)

# run tests on base packages
"test.base.package.base.examples" <- function() checkTrue("runPackageExamples"(pkg = "base"))
"test.base.package.datasets.examples" <- function() checkTrue("runPackageExamples"(pkg = "datasets"))
"test.base.package.graphics.examples" <- function() checkTrue("runPackageExamples"(pkg = "graphics"))
"test.base.package.grDevices.examples" <- function() checkTrue("runPackageExamples"(pkg = "grDevices"))
"test.base.package.methods.examples" <- function() checkTrue("runPackageExamples"(pkg = "methods"))
"test.base.package.parallel.examples" <- function() checkTrue("runPackageExamples"(pkg = "parallel"))
"test.base.package.stats4.examples" <- function() checkTrue("runPackageExamples"(pkg = "stats4"))
"test.base.package.tools.examples" <- function() checkTrue("runPackageExamples"(pkg = "tools"))
"test.base.package.utils.examples" <- function() checkTrue("runPackageExamples"(pkg = "utils",types=c("examples", "vignettes")))
"test.base.package.tcltk.examples" <- function()
{
  # check tcltk if not on LINUX
  if (!length(grep("linux",version$os))) checkTrue("runPackageExamples"(pkg = "tcltk")) else TRUE
}
# test the following only if MKL libraries are NOT installed:
if (identical(system.file(package="RevoUtilsMath"), ""))
{
	"test.base.package.grid.examples" <- function() checkTrue("runPackageExamples"(pkg = "grid"))
	"test.base.package.splines.examples" <- function() checkTrue("runPackageExamples"(pkg = "splines"))
	"test.base.package.stats.examples" <- function() checkTrue("runPackageExamples"(pkg = "stats"))
}


# run recommended package tests
"test.recommended.package.boot.examples" <- function() checkTrue("runPackageExamples"(pkg ="boot"))
"test.recommended.package.class.examples" <- function() checkTrue("runPackageExamples"(pkg = "class"))
"test.recommended.package.codetools.examples" <- function() checkTrue("runPackageExamples"(pkg = "codetools"))
"test.recommended.package.foreign.examples" <- function() checkTrue("runPackageExamples"(pkg = "foreign"))
"test.recommended.package.KernSmooth.examples" <- function() checkTrue("runPackageExamples"(pkg = "KernSmooth"))
"test.recommended.package.lattice.examples" <- function() checkTrue("runPackageExamples"(pkg = "lattice"))
"test.recommended.package.nnet.examples" <- function() checkTrue("runPackageExamples"(pkg = "nnet"))
"test.recommended.package.rpart.examples" <- function() checkTrue("runPackageExamples"(pkg = "rpart"))
"test.recommended.package.spatial.examples" <- function() checkTrue("runPackageExamples"(pkg = "spatial"))
"test.recommended.package.survival.examples" <- function() checkTrue("runPackageExamples"(pkg = "survival"))         
# test the following only if MKL libraries are NOT installed:
if (identical(system.file(package="RevoUtilsMath"), ""))
{
	"test.recommended.package.cluster.examples" <- function() checkTrue("runPackageExamples"(pkg = "cluster"))
	"test.recommended.package.MASS.examples" <- function() checkTrue("runPackageExamples"(pkg = "MASS"))
	"test.recommended.package.mgcv.examples" <- function() checkTrue("runPackageExamples"(pkg = "mgcv"))
	"test.recommended.package.nlme.examples" <- function() checkTrue("runPackageExamples"(pkg = "nlme"))
}


# restore stored session state
"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}
