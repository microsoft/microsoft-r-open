# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession()

"encodings.stress.iconv" <- function()
{
    
    ## from iconv.Rd
    (x <- "fa\xE7ile")
    charToRaw(xx <- iconv(x, "latin1", "UTF-8"))
    
    iconv(x, "latin1", "ASCII")          #   NA
    iconv(x, "latin1", "ASCII", "?")     # "fa?ile"
    iconv(x, "latin1", "ASCII", "")      # "faile"
    iconv(x, "latin1", "ASCII", "byte")  # "fa<e7>ile"
}   
    
"encodings.stress.Rhelp" <- function(){
    ## Extracts from R help files
    (x <- c("Ekstr\xf8m", "J\xf6reskog", "bi\xdfchen Z\xfcrcher"))
    iconv(x, "latin1", "ASCII//TRANSLIT")
    iconv(x, "latin1", "ASCII", sub="byte")
}

"encodings.stress.delimMatch" <- function()
{    
    ## tests of match length in delimMatch
    x <- c("a{bc}d", "{a\xE7b}")
    ## tools::delimMatch(x)
    ## FOLLOWING LINE COMMENTED OUT: THROWS ERROR IN OS X BUT NOT WINDOWS
    ##checkException(delimMatch(x))
    xx <- iconv(x, "latin1", "UTF-8")
    tools::delimMatch(xx) ## 5 6 in latin1, 5 5 in UTF-8
}

"test.encodings.stress" <- function()
{
    res <- try(encodings.stress.iconv())
    checkTrue(!is(res, "try-error"), msg=paste("encodings stress test failed in iconv", str(res)))      
    res <- try(encodings.stress.Rhelp())
    checkTrue(!is(res, "try-error"), msg=paste("encodings stress test failed in R help", str(res)))    
    res <- try(encodings.stress.delimMatch())
    checkTrue(!is(res, "try-error"), msg=paste("encodings stress test failed in delimmatch", str(res)))
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

