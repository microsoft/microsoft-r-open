% file RODBC/man/sqlTypeInfo.Rd
% copyright (C) 2004-2013  B. D. Ripley
%
\name{setSqlTypeInfo}
\alias{setSqlTypeInfo}
\alias{getSqlTypeInfo}
\title{Specify or Query a Mapping of R Types to DBMS Types}
\description{
  Specify or retrieve a mapping of \R types to DBMS datatypes.
}
\usage{
setSqlTypeInfo(driver, value)

getSqlTypeInfo(driver)
}
\arguments{
  \item{driver}{A character string specifying the \code{DBMS_name} as returned
    by \code{\link{odbcGetInfo}}.  Optional for \code{getSqlTypeInfo}.}
  \item{value}{A named list with character values.  This should have
    names \code{"double"}, \code{"integer"}, \code{"character"} and
    \code{"logical"}, and values SQL types appropriate to the DBMS.}
}
\details{
  This information is used by \code{\link{sqlSave}} if it creates a
  table in the DBMS and is not overridden by arguments \code{typeInfo}
  or \code{varTypes}.  Mappings are included for MySQL, PostgreSQL,
  SQLite, Oracle, Mimer, DB2 on Windows, and the Microsoft SQL Server,
  Access, Excel and Dbase drivers.

  The SQL types chosen should be nullable to allow \code{NA}s to be
  represented.  (Bit and boolean types often are not.)
}
\value{
  For \code{setSqlTypeInfo} none.

  For \code{getSqlTypeInfo} with an argument, a named list.  Without an
  argument, a data frame.
}
\author{
  Brian Ripley
}
\seealso{
  \code{\link{sqlTypeInfo}}, \code{\link{sqlSave}}.
}
\examples{\dontrun{
getSqlTypeInfo()
getSqlTypeInfo("MySQL")
setSqlTypeInfo("Microsoft SQL Server",
               list(double="float", integer="int",
                    character="varchar(255)", logical="varchar(5)"))
}}
\keyword{IO}
\keyword{database}
