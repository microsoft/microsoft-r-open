% file RODBC/man/RODBC-package.Rd
% copyright (C) 1999-2013  M. Lapsley and B. D. Ripley
%
\name{RODBC-package}
\alias{RODBC}
\title{ODBC Database Connectivity}

\description{
  Package \pkg{RODBC} implements ODBC database connectivity.

  See the package manual for details of installation and use.  (This
  will show up as a vignette, and can be accessed \emph{via}
  \code{RShowDoc("RODBC", package="RODBC")}.)
}

\details{
  Two groups of functions are provided.  The mainly internal
  \code{odbc*} commands implement low-level access to the ODBC functions
  of similar name.  The \code{sql*} functions operate at a higher level
  to read, save, copy and manipulate data between data frames and SQL
  tables.  Many connections can be open at once to any combination of
  DSN/hosts.
}

\author{
  Michael Lapsley and Brian Ripley
}

\keyword{IO}
\keyword{database}
