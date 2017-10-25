#' Set default CRAN repository to MRAN snapshot date.
#'
#' @inheritParams checkpoint
#' @param online If TRUE, performs online validation checks. This can be set to FALSE for programming purposes. Internally, [checkpoint()] sets this value to FALSE when not scanning for packages.
#'
#' @export
#' @example /inst/examples/example_setSnapshot.R
#' 
#' @family checkpoint functions
#'
setSnapshot <- function(snapshotDate, online = TRUE){
  if (missing(snapshotDate) || is.null(snapshotDate)) return(getOption("repos"))
  mran <- mranUrl()
  repoDate <- paste0(mran, snapshotDate)

  if(online) if(is.404(repoDate)) stop(paste0("Invalid snapshot date."))
  options(repos = c(CRAN = repoDate))
  message(paste("Using CRAN mirror at", repoDate))
}


#' Read list of available snapshot dates from MRAN.
#' 
#' Returns vector of available dates from MRAN or local MRAN repository.
#' 
#' @param mranRootUrl MRAN root. This can be a URL, e.g. `https://mran.microsoft.com/snapshot/` or the path to a local MRAN repository, e.g.`file:///local/path`
#' 
#' @export
#' @return Character vector with dates of valid snapshots
#' @family checkpoint functions
getValidSnapshots <- function(mranRootUrl = mranUrl()){
  con <- tryUrl(mranRootUrl)
  on.exit(close(con))
  text <- if (inherits(con, "file")) {
    dir(summary(con)$description)
  } else {
    suppressWarnings(tryCatch(readLines(con, warn = TRUE), error = function(e) e))
  }
  if (inherits(text, "error")) {
    stop(sprintf("Unable to download from MRAN: %s", 
                 text$message))
  }
  ptn <- "\\d{4}-\\d{2}-\\d{2}"
  idx <- grep(ptn, text)
  gsub(sprintf("^<a href=.*?>(%s).*?</a>.*$", ptn), 
       "\\1", text[idx])
}

