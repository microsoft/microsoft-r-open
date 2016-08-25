# Parts Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession()

"reg.BLAS.stress" <- function()
{

    ## PR#4582 %*% with NAs
    stopifnot(is.na(NA %*% 0), is.na(0 %*% NA))
    ## depended on the BLAS in use.


    ## found from fallback test in slam 0.1-15
    ## most likely indicates an inaedquate BLAS.
    x <- matrix(c(1, 0, NA, 1), 2, 2)
    y <- matrix(c(1, 0, 0, 2, 1, 0), 3, 2)
    (z <- tcrossprod(x, y))
    stopifnot(identical(z, x %*% t(y)))
    stopifnot(is.nan(log(0) %*% 0))
    ## depended on the BLAS in use: some (including the reference BLAS)
    ## had z[1,3] == 0 and log(0) %*% 0 as as.matrix(0).
}

"test.reg.BLAS.stress" <- function()
{
    res <- try(reg.BLAS.stress())
    checkTrue(!is(res, "try-error"), msg="reg.BLAS stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

