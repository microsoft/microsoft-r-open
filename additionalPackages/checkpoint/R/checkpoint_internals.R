
setMranMirror <- function(
  snapshotDate, 
  snapshotUrl = checkpoint:::getSnapshotUrl(snapshotDate)){
  options(repos = snapshotUrl)
}


setLibPaths <- function(checkpointLocation, libPath){
  newLoc <- c(libPath, 
              checkpointBasePkgs(checkpointLocation),
              .Library
              )
  assign(".lib.loc", newLoc, envir = environment(.libPaths))
}


#' Undo the effect of checkpoint by resetting .libPath to user library location.
#' 
#' @description 
#' 
#' This is an experimental solution to the situation where a user no longer wants to work in the checkpointed environment. The function resets [.libPaths] to point two libraries defined by the environment variable `R_Libs_User` and the R variable `.Library`.
#' 
#' Note that this does not undo any of the other side-effects of [checkpoint()]. Specifically, all loaded packages remain loaded, and the value of `getOption("repos")` remains unchanged.
#' 
#' @param new The new user library location. Defaults to `c(Sys.getenv("R_Libs_User"), .Library)`. See also [.libPaths()]
#' 
#' @export
#' @family checkpoint functions
unCheckpoint <- function(new = c(Sys.getenv("R_LIBS_USER"), .Library)){
  assign(".lib.loc", new, 
         envir = environment(.libPaths))
}


# Simple wrapper around `message()`, that only displays a message if verbose=TRUE
mssg <- function(verbose, ...) if(verbose) message(...)

correctR <- function(x) compareVersion(as.character(utils::packageVersion("base")), x) == 0


# Scans for R files in a folder and the first level subfolders.
anyRfiles <- function(path = ".", filenames = FALSE){
  findRfiles <- function(path = "."){
    pattern <- "\\.[rR]$|\\.[rR]nw$|\\.[rR]md$|\\.[rR]pres$|\\.[rR]proj$"
    list.files(path = path, pattern = pattern, full.names = TRUE)
  }
  dirs <- list.dirs(path = path, recursive = FALSE)
  rfilesInDirs <- as.vector(unlist(sapply(dirs, findRfiles)))
  rfiles <- findRfiles(path = path)
  allFiles <- c(rfiles, rfilesInDirs)
  if(filenames) allFiles else length(allFiles) > 0
}


# Use a simple heuristic to decide if the project looks like an R project.
validateProjectFolder <- function(project) {
  c1 <- normalizePath(project, winslash = "/") == normalizePath("~/", winslash = "/")
  c2 <- !anyRfiles(project)
  if(c1 && c2){
    message("This doesn't look like an R project directory.\n", 
            "Use forceProject = TRUE to force scanning"
    )
    answer = readline("Continue (y/n)? ")
    if(tolower(answer) != "y"){
      stop("Scanning stopped.", call. = FALSE)
    }
  }
}
