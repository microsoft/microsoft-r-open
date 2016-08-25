###
# Revolution R Enterprise package examples RUnit test functions (unix)
###

"checkPackageExamples" <- 
function (pkg,  outDir = file.path(tempdir(), "runPackageExamplesDir"), types = c("examples", "tests", "vignettes")) 
{
    if (!(file.exists(outDir) && file.info(outDir)$isdir)) {
        dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(outDir))
    }
    if (compareVersion(paste(R.version$major, R.version$minor, sep = ".") , "2.9.2") >= 0)
        tools::testInstalledPackage(pkg = pkg, lib.loc = .Library, outDir = outDir, types = types) == 0L
    else TRUE
}

