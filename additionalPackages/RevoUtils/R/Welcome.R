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
    RevoURL <- "https://go.microsoft.com/fwlink/?linkid=858763"
    if ((onWindows && isXP()) || (!onWindows && capabilities("X11") == FALSE)) {
        cat("For the latest information on Microsoft R Services and related products,\nenter the following URL in your browser:\n",
            RevoURL, "\n")
    }
    else {
        browseURL(RevoURL)
    }   
    invisible(RevoURL)
}
"readme" <- function()
{
	onWindows <- .Platform$OS.type == "windows" 
	clientURL <- "https://go.microsoft.com/fwlink/?linkid=858764"
	serverURL <- "https://go.microsoft.com/fwlink/?linkid=858760"
    winComUrl <- linuxComUrl <- "https://go.microsoft.com/fwlink/?linkid=858751"
    outUrl <- ""
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
        outUrl <- serverURL
        if (!onWindows && capabilities("X11")==FALSE) {
            cat("For the latest information on Microsoft R Server, enter the following \nURL in your browser:\n",
                serverURL, "\n")
        }
        else {
            browseURL(serverURL)
        }
    } else if (identical(RevoEdition, "Microsoft R Client")) {
        outUrl <- clientURL
		if (!onWindows && capabilities("X11")==FALSE) {
            cat("For the latest information on Microsoft R Client, enter the following \nURL in your browser:\n",
                clientURL, "\n")
        }
        else {
            browseURL(clientURL)
        }
    }
    else {
        outUrl <- winComUrl
		if (!onWindows && capabilities("X11")==FALSE) {
            cat("For the latest information on Microsoft R Open, enter the following \nURL in your browser:\n",
                winComUrl, "\n")
        }
        else {
            browseURL(winComUrl)
        }
    }
    invisible(outUrl)
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

"RevoLicense" <- function(pager=getOption("pager"), encoding="")
{
	haveRevoScaleR <- !identical(system.file("DESCRIPTION", package="RevoScaleR") , "")
    onWindows <- .Platform$OS.type == "windows"
    if (!onWindows) {
        if (missing(encoding)) {
            encoding <- "latin1"
        }
    }
    if (haveRevoScaleR) {
	    if (!isMicrosoftRClient()){
			RevoEdition <- "Microsoft R Server"
		} else {
			RevoEdition <- "Microsoft R Client"
		}
    } else {
        RevoEdition <- "Microsoft R Open"
    }
    filename <-  if (identical(RevoEdition, "Microsoft R Server")) {
            if (onWindows){
                file.path(Revo.home("licenses"), "MicrosoftRServerWindows")
            } else {
                file.path(Revo.home("licenses"),"MicrosoftRServerLicense")
            }
         } else if (identical(RevoEdition, "Microsoft R Client")) {
			 file.path(Revo.home("licenses"), "MicrosoftRClientLicense")
		 } else if (!identical(system.file(package="RevoUtilsMath"), "")){
             system.file("licenses/MicrosoftRServicesMKLLicense", package="RevoUtilsMath")
         } else {
             file.path(R.home(), "COPYING")
         }
    if (onWindows && length(grep("Microsoft", filename)) > 0) {
	   filename <- paste(filename, ".txt", sep="")
	   if (!hasArg("pager")) { 
		 pager <- "notepad"
         }
    }
    file.show(filename, pager=pager, encoding=encoding)
    invisible(filename)
}
