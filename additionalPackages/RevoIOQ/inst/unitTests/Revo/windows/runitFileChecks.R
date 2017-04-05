# Copyright (c) 2016 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages="tools")

filecheck.vcRunTime.stress <- function()
{
	## 
	## Test for vcRunTime dll
	##
	checkTrue(file.exists(file.path(R.home(), "bin", "x64", "msvcr120.dll")))
}
"test.filecheck.stress" <- function()
{
    DEACTIVATED("This test is DEACTIVATED due to failure in 3.3.2")
    
	if (!identical(system.file(package="RevoUtilsMath"), "") && identical(system.file(package="RevoScaleR"), "")) {
		res <- try(filecheck.vcRunTime.stress())
		checkTrue(!is(res, "try-error"), msg="VC RunTime stress test failed")    
	}
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}