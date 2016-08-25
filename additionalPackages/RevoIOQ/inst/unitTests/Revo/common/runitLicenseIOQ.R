# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages="tools")

isWindows <- .Platform$OS.type=="windows"

if (!exists("Revo.home"))
{
	Revo.home <- R.home
}
license.stress.Revo <- function()
{
    ##
    ## Testing for existence of basic Revo Licenses
    ##
    path <- Revo.home("licenses")
	haveRevoScaleR <- !identical(system.file("DESCRIPTION", package="RevoScaleR") , "")
    if (haveRevoScaleR) {
	    if (!isMicrosoftRClient()){
			RevoEdition <- "Microsoft R Server"
		} else {
			RevoEdition <- "Microsoft R Client"
		}
	}
    if (identical(RevoEdition, "Microsoft R Server")){
        checkTrue(file.exists(file.path(path, ifelse(isWindows, "MicrosoftRServerLicense.txt", "MicrosoftRServerLicense"))))
    } else {
		checkTrue(file.exists(file.path(path, ifelse(isWindows, "MicrosoftRClientLicense.txt", "MicrosoftRClientLicense"))))
	}
}

"test.AllLicense.stress" <- function()
{
    if (identical(Revo.home(), R.home()) )
    {
        DEACTIVATED("Test deactivated because Microsoft R Services components are not installed on this system.")
    }
    res <- try(license.stress.Revo())
    checkTrue(!is(res, "try-error"), msg="License stress test failed") 
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}


