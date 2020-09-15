\name{iread.table}
\alias{iread.table}
\title{Iterator over Rows of a Data Frame Stored in a File}
\description{
  Returns an iterator over the rows of a data frame stored in
  a file in table format.
  It is a wrapper around the standard \code{read.table} function.
}
\usage{
iread.table(file, ..., verbose=FALSE)
}
\arguments{
  \item{file}{the name of the file to read the data from.}
  \item{\dots}{all additional arguments are passed on to the
           \code{read.table} function.  See the documentation for
           \code{read.table} for more information.}
  \item{verbose}{logical value indicating whether or not to print the
           calls to \code{read.table}.}
}
\value{
  The file reading iterator.
}
\note{
In this version of \code{iread.table}, both the \code{read.table}
arguments \code{header} and \code{row.names} must be specified.  This is
because the default values of these arguments depend on the contents of
the beginning of the file.  In order to make the subsequent calls to
\code{read.table} work consistently, the user must specify those
arguments explicitly.  A future version of \code{iread.table} may remove
this requirement.
}
\seealso{
  \code{\link[utils]{read.table}}
}
\keyword{utilities}
