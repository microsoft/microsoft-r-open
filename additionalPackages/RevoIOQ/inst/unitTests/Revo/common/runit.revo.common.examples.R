###
# Revolution R Enterprise  package examples RUnit test functions (common)
###

"checkPackageExamples" <- 
function (pkg, outDir = file.path(tempdir(), "runPackageExamplesDir"), types = c("examples", "tests", "vignettes")) 
{
    if (!(file.exists(outDir) && file.info(outDir)$isdir)) {
        dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(outDir))
    }
    if (compareVersion(paste(R.version$major, R.version$minor, sep = "."), "2.9.2") >= 0)
        tools::testInstalledPackage(pkg = pkg, lib.loc = .libPaths(), outDir = outDir, types = types) == 0L
    else TRUE
}
if (!identical(system.file(package="foreach"), "")) 
{
    "test.revoparallelr.foreach.examples" <- function() checkTrue("checkPackageExamples"(pkg = "foreach"))
    "test.revoparallelr.iterators.examples" <- function() checkTrue("checkPackageExamples"(pkg = "iterators"))
}
if (!identical(system.file(package="RevoUtilsMath"), ""))
{
    "test.revor.RevoUtilsMath.examples" <- function() checkTrue("checkPackageExamples"(pkg = "RevoUtilsMath"))
}
if (!identical(system.file(package="RevoUtils"), ""))
{
    "test.revor.RevoUtils.examples" <- function() checkTrue("checkPackageExamples"(pkg = "RevoUtils"))
}
if (!identical(system.file(package="checkpoint"), ""))
{
    if (!identical(system.file(package="knitr"), "")) 
	{
		"test.revor.checkpoint.examples" <- function() checkTrue("checkPackageExamples"(pkg = "checkpoint"))
	} 
	else
	{
		"test.revor.checkpoint.examples" <- function() checkTrue("checkPackageExamples"(pkg = "checkpoint", types=c("examples", "tests")))
	}
}