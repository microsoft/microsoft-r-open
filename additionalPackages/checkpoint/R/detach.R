# Given a character vector with package names, construct regular expression that matches search()
makeSearchString <- function(pkg, search = TRUE) {
  regex <- "package:%s"
  if(search) regex <- paste0(regex, "$")
  paste(sprintf(regex, pkg), collapse="|")
}

makeDetachString <- function(pkg) makeSearchString(pkg, search = FALSE)

# Determine if package is in search()
findInSearchPath <- function(pkg){
  s <- search()
  z <- unlist(
    regmatches(s, gregexpr(makeSearchString(pkg), s))
  )
  setdiff(gsub("package:", "", z), "")
}

# Detach package from search()
detachFromSearchPath <- function(p){
  max.n <- 1 + length(p) # Prevent endless loop as a failsafe
  n <- 0
  repeat {
    d <- findInSearchPath(p)
    if(length(d) == 0) break # Stop if all packages unloaded
    for(to.detach in d){
      try({
        suppressWarnings(
          detach(name = makeDetachString(to.detach), unload = TRUE, 
                 force = TRUE, character.only = TRUE)
        )},
        silent = TRUE
      )
    }
    n <- n + 1
    if(n > max.n) break
  }
}
