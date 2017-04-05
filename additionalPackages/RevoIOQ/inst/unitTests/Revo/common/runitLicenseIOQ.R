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
    path <- system.file("licenses", package="MicrosoftR")
    checkTrue(file.exists(file.path(path,  "MicrosoftRServerLicense.txt")))
	checkTrue(file.exists(file.path(path, "MicrosoftRServerLicense")))
	checkTrue(file.exists(file.path(path, "MicrosoftRClientLicense.txt")))
	checkTrue(file.exists(file.path(path, "MicrosoftRClientLicense")))
	checkTrue(file.exists(file.path(path, "MicrosoftRServerWindows.txt")))
}

"test.AllLicense.stress" <- function()
{
    res <- try(license.stress.Revo())
    checkTrue(!is(res, "try-error"), msg="License stress test failed") 
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}


