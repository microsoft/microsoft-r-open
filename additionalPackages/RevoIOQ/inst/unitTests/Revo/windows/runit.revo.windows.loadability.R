###
# Revolution R Enterprise 2.0.0 package loadability RUnit test functions (windows)
###

"checkPackageLoadability" <- 
function (pkg, quiet) 
{
    RevoIOQ:::testPackageLoadability(pkg = pkg, lib.loc = .Library, quiet = quiet)
}
if (length(grep("Enterprise", Revo.version$version.string)) >  0){
    if (!RevoIOQ:::isComputeNode()){
         "test.revoide.pkgXMLBuilder.loadability" <- function() checkTrue("checkPackageLoadability"(pkg = "pkgXMLBuilder", quiet = TRUE))
         "test.revoide.XML.loadability" <- function() checkTrue("checkPackageLoadability"(pkg = "XML", quiet = TRUE))
    }
}
