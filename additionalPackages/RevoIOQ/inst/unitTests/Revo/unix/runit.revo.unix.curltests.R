###
# Microsoft R package version RUnit test functions (unix)
###

session <- RevoIOQ:::saveRUnitSession(packages="tools")

"curl.basicTest" <- function()
{
    # If this fails, Bug 292586 is probably still present
    basicOut <- curl::curl_fetch_memory("https://www.microsoft.com")
}
"test.curl.basicTest" <- function()
{
    res <- try(curl.basicTest())
    checkTrue(!is(res, "try-error"), msg="curl.basicTest failed--See Bug 292586")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}
