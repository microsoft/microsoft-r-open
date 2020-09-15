# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession()

"ok.errors.stress" <- function()
{
    
    #### STRICT test suite in the spirit of no-segfaults,
    #### but with explicit statements.
    
    (options(error=expression(NULL)))
    checkException(stop("test of `options(error=expression(NULL))'"))
    
    if(FALSE) {
        ## these ought to work on machines with enough memory
        ## These segfaulted in 1.3.x ,  give "could not allocate" errors now
        integer(2^30+1)
        double(2^30+1)
        complex(2^30+1)
        character(2^30+1)
        vector("list", 2^30+2)
    }
    
    Sys.getenv("USER") # should produce correct error message.
    
    ## bad infinite recursion / on.exit / ... interactions
    bar <- function() 1+1
    foo <- function() { on.exit(bar()); foo() }
    checkException(foo()) # now simple "infinite recursion"
}

"test.ok.errors.stress" <- function()
{
    res <- try(ok.errors.stress())
    checkTrue(!is(res, "try-error"), msg="ok.errors stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

