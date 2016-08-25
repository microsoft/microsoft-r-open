# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession()

"datasets.stress" <- function()
{
    
    #### Simple integrity tests of the system datasets
    
    options(useFancyQuotes=FALSE)
    env <- as.environment("package:datasets")
    d <- ls(env) # don't want .names
    for(f in d) {
        cat("\n** structure of dataset ", f, "\n", sep="")
        str(get(f, envir=env, inherits=FALSE))
    }
}

"test.datasets.stress" <- function()
{
    res <- try(datasets.stress())
    checkTrue(!is(res, "try-error"), msg="datasets stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

