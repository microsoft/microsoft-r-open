###
# Revolution R Enterprise package version RUnit test functions (unix)
###

"checkPackageVersion" <- 
function (pkg, ver) 
{
    RevoIOQ:::testPackageVersion(pkg, ver, lib.loc = .Library)
}

