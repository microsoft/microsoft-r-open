# These functions mock installation of packages, for fast testing purposes only

is_mock_environment <- function()Sys.getenv("checkpoint.mock.install") == TRUE
set_mock_environment <- function()Sys.setenv("checkpoint.mock.install" = TRUE)
reset_mock_environment <- function()Sys.setenv("checkpoint.mock.install" = FALSE)

lib_in_tempdir <- function(lib){
  np <- function(x)normalizePath(x, winslash = "/")
  grepl(np(tempdir()), np(lib))
}

install.packages <- function(pkgs, lib = .libPaths()[1], repos = getOption("repos"), ...){
  if(is_mock_environment() && lib_in_tempdir(lib)){
    mock.install.packages(pkgs = pkgs, lib = lib, repos = repos, ...)
  } else {
    capture.output(
      z <- utils::install.packages(pkgs = pkgs, lib = lib, repos = repos, ...)
    )
    invisible()
  }
  
}

installed.packages <- function(lib.loc = .libPaths()[1], ...){
  if(is_mock_environment() && lib_in_tempdir(lib.loc)){
    mock.installed.packages(lib.loc = lib.loc,  ...)
  } else {
    utils::installed.packages(lib.loc = lib.loc,  ...)
  }
  
}


# mocks install.packages()
# writes a description file with three lines
mock.install.packages <- function(pkgs, lib = .libPaths()[1], repos = getOption("repos"), ...){
  do_one <- function(pkg){
    np <- function(x)normalizePath(x, winslash = "/")
    stopifnot(grepl(np(tempdir()), np(lib)))
    p <- available.packages()[pkg, ]  
    msg <- paste0("Package: ", pkg, "\n", "Version: ", p["Version"], "\n", "Description: ", pkg)
    fp <- file.path(lib, pkg)
    # unlink(fp, recursive = TRUE)
    dir.create(fp, recursive = TRUE, showWarnings = FALSE)
    message("Mocking Content type 'application/zip' length 0 bytes (0 KB)")
    cat(msg, file = file.path(fp, "DESCRIPTION"))
  }
  for(pkg in pkgs)do_one(pkg)
  invisible()
}


# mocks installed.packages()
mock.installed.packages <- function(lib.loc = .libPaths()[1], priority, ...){
  if(!missing(priority) && priority == "base") {
    utils::installed.packages(lib.loc = lib.loc, priority = priority, ...)
  }
  f <- list.files(lib.loc, pattern = "DESCRIPTION$", recursive = TRUE, full.names = FALSE)
  f <- f[grepl("^\\w*/DESCRIPTION$", f)]
  p <- dirname(f)
  pdb <- available.packages()
  pdb[pdb[, "Package"] %in% p, ]
}
