###
# Revolution R Enterprise package version RUnit test functions (common)
###

"checkPackageVersion" <-
function (pkg, ver)
{
    RevoIOQ:::testPackageVersion(pkg, ver, lib.loc = .libPaths())
}
revover <- paste(unclass(packageVersion("RevoIOQ"))[[1]], collapse=".")
if (!identical(system.file(package="RevoScaleR") , "")) {
    "test.revor.RevoTreeView.version" <- function() checkTrue("checkPackageVersion"(pkg = "RevoTreeView", ver = "10.0.0"))
    "test.revor.RevoPemaR.version" <- function()
    {
        checkTrue("checkPackageVersion"(pkg = "RevoPemaR", ver = "10.0.0"))
    }
    "test.revor.doRSR.version" <- function() checkTrue("checkPackageVersion"(pkg = "doRSR", ver = "10.0.0"))
    "test.revor.RevoUtilsMath.version" <- function() checkTrue("checkPackageVersion"(pkg = "RevoUtilsMath", ver = "10.0.1"))
}
if (!identical(system.file(package="foreach") , "")) {
    "test.revoparallelr.foreach.version" <- function() checkTrue("checkPackageVersion"(pkg = "foreach", ver = "1.5.0"))
    "test.revoparallelr.iterators.version" <- function() checkTrue("checkPackageVersion"(pkg = "iterators", ver = "1.0.10"))
}
if (!identical(system.file(package="RevoMods"), ""))
{
    "test.revor.RevoIOQ.version" <- function() checkTrue("checkPackageVersion"(pkg = "RevoIOQ", ver = revover))
    "test.revor.RUnit.version" <- function() checkTrue("checkPackageVersion"(pkg = "RUnit", ver = "0.4.26"))
    "test.revoparallelr.doParallel.version" <- function() checkTrue("checkPackageVersion"(pkg = "doParallel", ver = "1.0.13"))
    "test.revor.RevoUtils.version" <- function() checkTrue("checkPackageVersion"(pkg = "RevoUtils", ver = "11.0.0"))
    "test.revor.RevoMods.version" <- function() checkTrue("checkPackageVersion"(pkg = "RevoMods", ver = "11.0.0"))
} else
{
    if (!identical(system.file(package="RevoUtilsMath"),""))
	{
        "test.revor.RevoUtilsMath.version" <- function() checkTrue("checkPackageVersion"(pkg = "RevoUtilsMath", ver = "11.0.0"))   
    } 
	if (!identical(system.file(package="RevoUtils"),""))
	{
		"test.revor.RevoUtils.version" <- function() checkTrue("checkPackageVersion"(pkg = "RevoUtils", ver = "11.0.0"))
	}
    if (!identical(system.file(package="doParallel"),""))
	{
        "test.revoparallelr.doParallel.version" <- function() checkTrue("checkPackageVersion"(pkg = "doParallel", ver = "1.0.13"))
	}
}