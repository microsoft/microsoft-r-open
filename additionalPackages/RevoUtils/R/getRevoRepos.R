"getRevoRepos" <- function(version = paste(unlist(unclass(getRversion()))[1:2], collapse="."), CRANmirror=FALSE, MRANmirror=FALSE, httpsSupported=TRUE)
{
	httpPrefix <- ifelse(httpsSupported, "https://", "http://")
	if (CRANmirror)
	{
		return(paste0(httpPrefix, "cran.revolutionanalytics.com"))
	}
	else 
    {
        snapshotDate <- utils::packageDescription("RevoUtils", fields="MRANDate")
        return(paste0(httpPrefix, "mran.revolutionanalytics.com/snapshot/", snapshotDate))
    }
}

"httpsSupported" <- function (mran = "https://mran.revolutionanalytics.com/snapshot", method=getOption("download.file.method"))
{
    tf <- tempfile()
    on.exit(unlink(tf))
    pdb <- suppressWarnings({
        testfile <- paste0(mran, if (grepl("snapshot$", mran))
            "/2015-09-01/src/contrib/checkTimings.html"
        else "/src/contrib/checkTimings.html")
        try(utils::download.file(url = testfile, destfile = tf, method = method,
            mode = "w", cacheOK = FALSE, quiet = TRUE), silent = TRUE)
    })
    if (!inherits(pdb, "try-error")) {
        return(TRUE)
    }
    FALSE
}
