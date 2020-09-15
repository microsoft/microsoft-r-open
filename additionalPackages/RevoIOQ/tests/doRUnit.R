if (require("RUnit", quietly=TRUE)) 
{ 
  library("RevoIOQ")
  if (!RevoIOQ:::RevoIOQ(view=FALSE))
  {
    stop("RUnit failures/errors were encountered during the running of RevoIOQ. See test reports for more information.")
  }
} else {
  warning("cannot run unit tests -- package RUnit is not available")
}
