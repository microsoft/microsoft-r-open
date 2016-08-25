###
# Revolution R Enterprise package loadability RUnit test functions (common)
###

"checkPackageLoadability" <- 
function (pkg, quiet) 
{
    RevoIOQ:::testPackageLoadability(pkg = pkg, lib.loc = .libPaths(), quiet = quiet)
}

"test.revor.RUnit.loadability" <- function() checkTrue("checkPackageLoadability"(pkg = "RUnit", quiet = TRUE))

if (!identical(system.file(package="RevoScaleR"), "")) 
{
    "test.revor.RevoTreeView.loadability" <- function() checkTrue("checkPackageLoadability"(pkg = "RevoTreeView", quiet = TRUE))   
    "test.revor.RevoPemaR.loadability" <- function() checkTrue("checkPackageLoadability"(pkg = "RevoPemaR", quiet = TRUE))
 
} 
if (!identical(system.file(package="foreach"), "")) 
{
    "test.revoparallelr.foreach.loadability" <- function() checkTrue("checkPackageLoadability"(pkg = "foreach", quiet = TRUE))
    "test.revoparallelr.iterators.loadability" <- function() checkTrue("checkPackageLoadability"(pkg = "iterators", quiet = TRUE))
	if (!identical(system.file(package="doParallel"), "")) {
		"test.revor.doParallel.loadability" <- function() checkTrue("checkPackageLoadability"(pkg = "doParallel", quiet = TRUE)) 
	}
}
if (!identical(system.file(package="RevoMods"), ""))
{
    "test.revor.RevoMods.loadability" <- function() checkTrue("checkPackageLoadability"(pkg = "RevoMods", quiet = TRUE))   
    "test.revor.RevoIOQ.loadability" <- function() checkTrue("checkPackageLoadability"(pkg = "RevoIOQ", quiet = TRUE)) 
    "test.revor.RUnit.loadability" <- function() checkTrue("checkPackageLoadability"(pkg = "RUnit", quiet = TRUE))
}

 
if (!identical(system.file(package="RevoUtilsMath"),""))
{
	"test.revor.RevoUtilsMath.loadability" <- function() checkTrue("checkPackageLoadability"(pkg = "RevoUtilsMath",  quiet = TRUE))  
} 
if (!identical(system.file(package="RevoUtils"),""))
{
	"test.revor.RevoUtils.loadability" <- function() checkTrue("checkPackageLoadability"(pkg = "RevoUtils",  quiet = TRUE))
}
if (!identical(system.file(package="checkpoint"),""))
{
	"test.revor.checkpoint.loadability" <- function() checkTrue("checkPackageLoadability"(pkg = "checkpoint",  quiet = TRUE))
}