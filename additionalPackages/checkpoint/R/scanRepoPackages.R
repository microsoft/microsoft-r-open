knitr.is.installed <- function()length(find.package(package="knitr", quiet = TRUE)) > 0

projectScanPackages <- function(...){
  warning("Use checkpoint:::scanForPackages() instead")
  scanForPackages(...)
}

#' Scans a project (or folder) for references to packages.
#' 
#' @inheritParams checkpoint
#' @return A list with two elements:
#' * pkgs: a character vector with the names of identified packages
#' * error: a character vector with information about files that could not be parsed
#' @export
scanForPackages <- function(project = getwd(), verbose = TRUE, 
                            use.knitr = FALSE, 
                            auto.install.knitr = FALSE, 
                            scan.rnw.with.knitr = FALSE
                          ){
  # detect all package dependencies for a project
  dir <- normalizePath(project, winslash='/', mustWork=FALSE)
  pattern <- if(!use.knitr) "\\.[rR]$|\\.[rR]nw$" else
    "\\.[rR]$|\\.[rR]nw$|\\.[rR]md$|\\.[rR]pres$"
  
  if(scan.rnw.with.knitr){
    ext_r <- c("R")
    ext_k <- c("Rmd", "Rpres", "Rhmtl", "Rnw") # knitr / rmarkdown extensions
  } else {
    ext_r <- c("R", "Rnw")
    ext_k <- c("Rmd", "Rpres", "Rhmtl") # knitr / rmarkdown extensions
  }
  
  makePtn <- function(x)sprintf("\\.(%s)$", paste(c(x, tolower(x)), collapse="|"))
  
  files_r <- list.files(dir, pattern = makePtn(ext_r), 
                        ignore.case = TRUE, recursive = TRUE)
  files_k <- list.files(dir, pattern = makePtn(ext_k), 
                        ignore.case = TRUE, recursive = TRUE)
  
  R_files <- files_r
  
  if(length(files_k) > 0) {
    if(use.knitr) {
      if(!knitr.is.installed()) {
        mssg(verbose, "The knitr package is not available and Rmarkdown files will not be parsed")
      } else {
        R_files <- c(files_r, files_k)
      }
    } else {
      mssg(verbose, "rmarkdown files found and will not be parsed. Set use.knitr = TRUE")
    }
  }
  
  if(length(R_files) == 0){
    list(pkgs = character(), error = character())
  } else {
    if(interactive()){
      z <- lapplyProgressBar(R_files, deps_by_ext, dir=dir, verbose=verbose,
                             scan.rnw.with.knitr = scan.rnw.with.knitr)
    } else {
      z <- lapply(R_files, deps_by_ext, dir=dir, verbose=verbose, 
                  scan.rnw.with.knitr = scan.rnw.with.knitr)
    }
    
    pkgs <- sort(unique(do.call(c, lapply(z, "[[", "pkgs"))))
    if(length(files_k) > 0 && auto.install.knitr) {
      pkgs <- unique(c(pkgs, "knitr"))
    }
    error <- sort(unique(do.call(c, lapply(z, "[[", "error"))))
    error <- gsub(sprintf("%s[//|\\]*", dir), "", error)
    list(pkgs = pkgs, error = error)
  }
  
}


# Wraps lapply() in a progress bar, if the session is interactive and the list contains more than 10 elements
lapplyProgressBar <- function(X, FUN, ...){
  if(interactive() && length(X) >= 10){
    env <- environment()
    N <- length(X)
    counter <- 0
    pb <- txtProgressBar(min = 0, max = N, style = 3)
    on.exit(close(pb))
    
    wrapper <- function(...){
      curVal <- get("counter", envir = env)
      assign("counter", curVal + 1, envir = env)
      setTxtProgressBar(get("pb", envir = env), curVal + 1)
      FUN(...)
    }
    lapply(X, wrapper, ...)
  } else {
    lapply(X, FUN, ...)
  }
}

getFileExtension <- function(filename)tolower(gsub(".*\\.", "", filename))



# ad-hoc dispatch based on the file extension
deps_by_ext <- function(file, dir, verbose = TRUE, scan.rnw.with.knitr = FALSE) {
  file <- file.path(dir, file)
  fileext <- getFileExtension(file)
  switch(fileext,
         r = deps.R(file, verbose = verbose),
         rnw = if(scan.rnw.with.knitr){
           deps.Rmd(file, verbose = verbose)
         } else {
           deps.Rnw(file, verbose = verbose)
         },
         rmd = deps.Rmd(file, verbose = verbose),
         rpres = deps.Rpres(file, verbose = verbose),
         txt = deps.txt(file, verbose = verbose),
         stop("Unrecognized file type '", file, "'")
  )
}

deps.Rmd <- deps.Rpres <- function(file, verbose=TRUE) {
  tempfile <- tempfile(fileext = ".Rmd")
  showErrors <- getOption("show.error.messages")
  options("show.error.messages" = FALSE)
  on.exit({unlink(tempfile); options("show.error.messages" = showErrors)})
  if(!knitr.is.installed()) stop("knitr is not installed")
  p <- try(
    suppressWarnings(
      knitr::knit(file, output = tempfile, tangle = TRUE, quiet = TRUE)
    ),
    silent = TRUE
  )
  
  if(inherits(p, "error")) {
    return(list(pkgs=character(), error=file))
  }
  
  p <- deps.R(tempfile)
  if(length(p[["error"]]) != 0 ) {
    p[["error"]] <- file
  }
  p
}

deps.Rnw <- function(file, verbose=TRUE) {
  tempfile <- tempfile(fileext = ".Rnw")
  showErrors <- getOption("show.error.messages")
  options("show.error.messages" = FALSE)
  on.exit({unlink(tempfile); options("show.error.messages" = showErrors)})
  p <- try(
    Stangle(file, output = tempfile, quiet = TRUE),
    silent = TRUE
  )
  if(inherits(p, "error")) {
    return(list(pkgs=character(), error=file))
  }
  
  p <- deps.R(tempfile)
  if(length(p[["error"]]) != 0 ) {
    p[["error"]] <- file
  }
  p
}

deps.R <- deps.txt <- function(file, verbose=TRUE) {
  if (!file.exists(file)) {
    mssg(verbose, "No file at path '", file, "'.")
    return(list(pkgs=character(), error=file))
  }
  
  # build a list of package dependencies to return
  pkgs <- character()
  
  # parse file and examine expressions
  p <- tryCatch({
    exprs <- suppressWarnings(parse(file, n = -1L))
    for (i in seq_along(exprs))
      pkgs <- append(pkgs, expressionDependencies(exprs[[i]]))
  }, error = function(e) e
  )
  if(inherits(p, "error")) {
    list(pkgs=character(), error=file)
  } else {
    list(pkgs=unique(pkgs), error=character())
  }
}

expressionDependencies <- function(e) {
  # base case
  if (is.atomic(e) || is.name(e)) return()
  
  # recursive case: expression (= list of calls)
  if (is.expression(e)) {
    return(unlist(lapply(e, expressionDependencies)))
  }
  
  # base case: a call
  fname <- as.character(e[[1L]])
  # a refclass method call, so return
  # if (length(fname) > 1) return()
  
  if (length(fname) == 1) {
    
    # base case: call to library/require
    if (fname %in% c("library", "require")) {
      mc <- match.call(get(fname, baseenv()), e)
      if (is.null(mc$package)) return(NULL)
      if (isTRUE(mc$character.only)) return(NULL)
      
      return(as.character(mc$package))
    }
    
    # base case: methods functions
    if (fname %in% c("setClass", "setRefClass", "setMethod", "setGeneric")) {
      return("methods")
    }
    
  } else {
    
    # base case: call to :: or :::
    if (fname[1] %in% c("::", ":::")) (
      return(as.character(fname[2]))
    )
  }
  
  # recursive case: all other calls
  children <- lapply(as.list(e[-1]), expressionDependencies)
  unique(unlist(children))
}
