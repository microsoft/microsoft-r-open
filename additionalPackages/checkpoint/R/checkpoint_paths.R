localSnapshots <- function(checkpointLocation = "~/"){
  cp <- checkpointPath(snapshotDate = "", checkpointLocation = checkpointLocation, type = "snapshot")
  ptn <- "\\d{4}-\\d{2}-\\d{2}"
  dir(cp, pattern = ptn)
}

checkpointPath <- function(snapshotDate, checkpointLocation,
                           type = c("lib", "src", "snapshot", "root", "base")){
  rootPath <- normalizePath(
    file.path(checkpointLocation, ".checkpoint"),
    mustWork = FALSE)
  type <- match.arg(type)
  if(type == "base") return(
    normalizePath(
      file.path(rootPath, paste0("R-", getRversion())),
      winslash = "/", mustWork = FALSE)
  )
  snapshotPath <- file.path(rootPath, snapshotDate)
  libPath <- file.path(snapshotPath, "lib", R.version$platform, base::getRversion())
  srcPath <- file.path(libPath, "src/contrib")
  normalizePath(
    switch(
      type,
      root  = rootPath,
      lib   = libPath,
      src   = srcPath,
      snapshot = snapshotPath
    ),
    winslash = "/",
    mustWork = FALSE)
}


createFolders <- function(snapshotDate, checkpointLocation){
  paths <- sapply(c("root", "lib", "src"), checkpointPath,
                  snapshotDate = snapshotDate,
                  checkpointLocation = checkpointLocation)
  sapply(paths, function(x) if(!file.exists(x)) dir.create(x, recursive=TRUE, showWarnings = FALSE))
  all(file.exists(paths))
}


file.path <- function(...){
  gsub(pattern = "/+", replacement = "/", x = base::file.path(...))
}


# If the folder doesn't exists, ask user permission to create it
# The argument interactive is purely for testing purposes
authorizeFileSystemUse <- function(checkpointLocation = "~/", 
                                   interactive) {
  if (missing(interactive)){
	isInteractive <- base::interactive()
	interactive <- isInteractive
  }
  checkpointRoot = file.path(checkpointLocation, ".checkpoint")
  if(file.exists(checkpointRoot)) {
    if(!file.info(checkpointRoot)$isdir)
      stop("Can't use a non-directory as checkpoint root")}
  else {
    if(interactive) {
      message("Can I create directory ", checkpointRoot, " for internal checkpoint use?\n")
      answer = readline("Continue (y/n)? ")
      if(tolower(answer) != "y")
        stop("Cannot proceed without access to checkpoint directory")}
    else {
      stop(paste(
        "The .checkpoint folder does not exist. Please try again after creating the folder at", 
        normalizePath(checkpointRoot, mustWork = FALSE)
      ))
    }
  }
  invisible()
}
