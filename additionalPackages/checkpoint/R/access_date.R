# Write a small file into the snapshot that contains the last date this snapshot was accessed by checkpoint()
setAccessDate <- function(snapshotDate, checkpointLocation = "~/"){
  today <- strftime(Sys.Date(), "%Y-%m-%d", tz = FALSE)
  if(missing(snapshotDate) || is.null(snapshotDate)){
    snapshotDate <- today
  }
  cpdir <- checkpointPath(snapshotDate, 
                          checkpointLocation = checkpointLocation, 
                          type = "snapshot")
  lastaccessFile <- file.path(cpdir, ".lastaccessed")
  writeLines(today, con = lastaccessFile)
}


#' Returns the date the snapshot was last accessed.
#' 
#' The [checkpoint()] function stores a marker in the snapshot folder every time the function gets called. This marker contains the system date, thus indicating the the last time the snapshot was accessed.
#' 
#' @inheritParams checkpoint
#' @return Named character with last access date
#' @export
#' @family checkpoint functions
#' @seealso [checkpointRemove()]
getAccessDate <- function(checkpointLocation = "~/"){
  cp <- checkpointPath(NULL, 
                       checkpointLocation = checkpointLocation, 
                       type = "root"
  )
  z <- dir(cp, pattern = ".{4}-.{2}-.{2}", 
           include.dirs = TRUE, 
           full.names = TRUE
  )
  sapply(z, function(x){
    laf <- file.path(x, ".lastaccessed")
    if(file.exists(laf)) readLines(laf) else NA
  })
}

