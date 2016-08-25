"isXP" <- function()
{
	length(grep("XP", utils::win.version())) > 0
}
"isRRE" <- function()
{
    !identical(system.file(package="RevoScaleR"), "")
}
"revo" <- function()
{
	onWindows <- .Platform$OS.type == "windows"
    RevoURL <- "http://www.revolutionanalytics.com/"
    if ((onWindows && isXP()) || (!onWindows && capabilities("X11") == FALSE)) {
        cat("For the latest information on Microsoft R Services and related products,\nenter the following URL in your browser:\n",
            RevoURL, "\n")
    }
    else {
        browseURL(RevoURL)
    }   
    invisible(NULL)
}
"readme" <- function()
{
	onWindows <- .Platform$OS.type == "windows" 
	clientURL <- "https://aka.ms/microsoft-r-client-release-notes"
	serverURL <- "https://aka.ms/microsoft-r-server-release-notes"
    winComUrl <- linuxComUrl <- "http://mran.revolutionanalytics.com/news/"
	haveRevoScaleR <- !identical(system.file("DESCRIPTION", package="RevoScaleR") , "")
    if (haveRevoScaleR) {
	    if (!RevoScaleR:::rxIsExpressEdition() || length(grep("SQL Server", normalizePath(R.home())))> 0){
			RevoEdition <- "Microsoft R Server"
		} else {
			RevoEdition <- "Microsoft R Client"
		}
    } else {
        RevoEdition <- "Microsoft R Open"
    }
	if (identical(RevoEdition, "Microsoft R Server")) {
		if (!onWindows && capabilities("X11")==FALSE) {
            cat("For the latest information on Microsoft R Server, enter the following \nURL in your browser:\n",
                serverURL, "\n")
        }
        else {
            browseURL(serverURL)
        }
    } else if (identical(RevoEdition, "Microsoft R Client")) {
		if (!onWindows && capabilities("X11")==FALSE) {
            cat("For the latest information on Microsoft R Client, enter the following \nURL in your browser:\n",
                clientURL, "\n")
        }
        else {
            browseURL(clientURL)
        }
    }
    else {
		if (!onWindows && capabilities("X11")==FALSE) {
            cat("For the latest information on Microsoft R Open, enter the following \nURL in your browser:\n",
                winComURL, "\n")
        }
        else {
            browseURL(winComURL)
        }
    }
    invisible(NULL)
}

"privacy" <- function()
{
	onWindows <- .Platform$OS.type == "windows" 
	privacyURL <- "https://aka.ms/microsoft-r-privacy"
    if (!onWindows && capabilities("X11")==FALSE) {
        cat("For the privacy policy, enter the following URL in your browser:\n",privacyURL)
    } else {
       browseURL(privacyURL) 
    }
}

"RevoLicense" <- function(pager=getOption("pager"))
{
	haveRevoScaleR <- !identical(system.file("DESCRIPTION", package="RevoScaleR") , "")
    if (haveRevoScaleR) {
	    if (!RevoScaleR:::rxIsExpressEdition() || length(grep("SQL Server", normalizePath(R.home())))> 0){
			RevoEdition <- "Microsoft R Server"
		} else {
			RevoEdition <- "Microsoft R Client"
		}
    } else {
        RevoEdition <- "Microsoft R Open"
    }
    filename <-  if (identical(RevoEdition, "Microsoft R Server")) {
             file.path(Revo.home("licenses"),"MicrosoftRServerLicense")
         } else if (identical(RevoEdition, "Microsoft R Client")) {
			 file.path(Revo.home("licenses"), "MicrosoftRClientLicense")
		 } else if (!identical(system.file(package="RevoUtilsMath"), "")){
             system.file("licenses/MicrosoftRServicesMKLLicense", package="RevoUtilsMath")
         } else {
             file.path(R.home(), "COPYING")
         }
    if (.Platform$OS.type=="windows" && length(grep("Microsoft", filename)) > 0) {
	   filename <- paste(filename, ".txt", sep="")
	   if (!hasArg("pager")) { 
		 pager <- "notepad"
         }
    }
    file.show(filename, pager=pager)
    invisible(NULL)
}
