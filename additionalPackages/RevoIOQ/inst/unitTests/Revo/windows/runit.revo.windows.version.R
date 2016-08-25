###
# Revolution R Enterprise 2.0.0 package version RUnit test functions (windows)
###

"checkPackageVersion" <-
function (pkg, ver)
{
    RevoIOQ:::testPackageVersion(pkg, ver, lib.loc = .Library)
}
if (length(grep("Enterprise", Revo.version$version.string)) >  0){
    if (!RevoIOQ:::isComputeNode()){
    	"test.revoide.pkgXMLBuilder.version" <- function() checkTrue("checkPackageVersion"(pkg = "pkgXMLBuilder", ver = "1.0"))
    	"test.revoide.revoIpe.version" <- function() checkTrue("checkPackageVersion"(pkg = "revoIpe", ver = "1.1"))
    	"test.revoide.XML.version" <- function() checkTrue("checkPackageVersion"(pkg = "XML", ver = "3.98-1.1"))
    }
}
