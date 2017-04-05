"isMicrosoftRClient" <- function() 
{
    returnVal <- FALSE
    if (identical(.Platform$OS.type, "windows")) {
        clientRegEntry <- try(utils::readRegistry(file.path("SOFTWARE", 
            "Microsoft", "R Client", fsep = "\\"), "HLM"), silent = TRUE)
        if (!inherits(clientRegEntry, "try-error")) {
                clientPath <- normalizePath(file.path(clientRegEntry$Path, "R_SERVER"))
                Rpath <- normalizePath(file.path(R.home()))
                if (identical(clientPath, Rpath)){
                    returnVal <- TRUE
                }
        }
    } else if(identical(.Platform$OS.type, "unix") && !identical(Sys.info()["sysname"], "Darwin")){
		if (!identical(system.file(package="RevoScaleR"), "")) {
			returnVal <- RevoScaleR:::rxIsExpressEdition()
		}
	}
    returnVal
}
