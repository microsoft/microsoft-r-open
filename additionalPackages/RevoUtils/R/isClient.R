"isMicrosoftRClient" <- function()
{
    returnVal <- FALSE
	if (identical(.Platform$OS.type, "windows")) {
		clientRegEntry <- try(utils::readRegistry(file.path("SOFTWARE", "Microsoft", "R Client", fsep="\\"), "HLM"), 
								  silent=TRUE)
		if (!inherits(clientRegEntry, "try-error"))
		{
			returnVal <- TRUE
		}
	}
	returnVal
}