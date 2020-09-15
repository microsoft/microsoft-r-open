###
# Revolution R Enterprise package loadability RUnit test functions (unix)
###

"checkPackageLoadability" <- 
function (pkg, quiet) 
{
    RevoIOQ:::testPackageLoadability(pkg = pkg, lib.loc = .Library, quiet = quiet)
}
