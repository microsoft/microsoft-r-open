is_online <- function(){
  u <- tryCatch(url("https://mran.microsoft.com"),
                error = function(e)e)
  if(inherits(u, "error")){
    u <- url("http://mran.microsoft.com")
  }
  on.exit(close(u))
  z <- tryCatch(suppressWarnings(readLines(u, n = 1, warn = FALSE)),
                error = function(e)e)
  !inherits(z, "error")
}


skip_if_offline <- function(){
  if(!is_online()) testthat::skip("Offline. Skipping test.")
}

make_fake_archive <- function(){
  td <- file.path(tempdir(), ".fakecheckpoint", ".checkpoint")
  unlink(td, recursive = TRUE)
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(td, "2099-01-01"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(td, "2099-01-02"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(td, "2099-01-03"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(td, "2099-01-04"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(td, "2099-01-05"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(td, "2099-01-06"), recursive = TRUE, showWarnings = FALSE)
  
  writeLines(con = file.path(td, "2099-01-01", ".lastaccessed"), "2099-03-01")
  writeLines(con = file.path(td, "2099-01-02", ".lastaccessed"), "2099-01-02") # untouched
  writeLines(con = file.path(td, "2099-01-03", ".lastaccessed"), "2099-04-01")
  writeLines(con = file.path(td, "2099-01-04", ".lastaccessed"), "2099-02-01")
  writeLines(con = file.path(td, "2099-01-05", ".lastaccessed"), "2099-06-01")
  writeLines(con = file.path(td, "2099-01-06", ".lastaccessed"), "2099-02-01")
  dirname(td)
}

