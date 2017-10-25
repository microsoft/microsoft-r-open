% file RODBC/man/odbcConnect.Rd
% copyright (C) 1999-2002  M. Lapsley
% copyright (C) 2002-2013  B. D. Ripley
%
\name{odbcConnect}
\alias{odbcConnect}
\alias{odbcDriverConnect}
\alias{odbcReConnect}
#ifdef windows
\alias{odbcConnectAccess}
\alias{odbcConnectDbase}
\alias{odbcConnectExcel}
\alias{odbcConnectAccess2007}
\alias{odbcConnectExcel2007}
#endif
\title{ODBC Open Connections}
\description{
  Open connections to ODBC databases.
}
\usage{
odbcConnect(dsn, uid = "", pwd = "", ...)

odbcDriverConnect(connection = "", case, believeNRows = TRUE,
                  colQuote, tabQuote = colQuote,
                  interpretDot = TRUE, DBMSencoding = "",
                  rows_at_time = 100, readOnlyOptimize = FALSE)

odbcReConnect(channel, ...)
#ifdef windows

odbcConnectAccess(access.file, uid = "", pwd = "", ...)
odbcConnectAccess2007(access.file, uid = "", pwd = "", ...)
odbcConnectDbase(dbf.file, ...)
odbcConnectExcel(xls.file, readOnly = TRUE, ...)
odbcConnectExcel2007(xls.file, readOnly = TRUE, ...)
#endif
}%"
\arguments{
  \item{dsn}{character string. A registered data source name.}
  \item{uid, pwd}{UID and password for authentication (if required).}
  \item{connection}{character string. See your ODBC documentation for the
    format.}
  \item{...}{further arguments to be passed to \code{odbcDriverConnect}.}
  \item{case}{Controls case changes for different DBMS engines.  See
    \sQuote{Details}.}
  \item{channel}{RODBC connection object returned by \code{odbcConnect}.}
  \item{believeNRows}{logical.  Is the number of rows returned by the
    ODBC connection believable?  Not true for some Oracle and Sybase drivers,
    apparently, nor for Actual Technologies' SQLite driver for Mac OS X.}
  \item{colQuote, tabQuote}{how to quote column (table) names in SQL
    statements.  Can be of length 0 (no quoting), a length--1 character
    vector giving the quote character to be used at both ends, or a
    length--2 character vector giving the beginning and ending quotes.
    ANSI SQL uses double quotes, but the default mode for a MySQL server
    is to use backticks.

    The defaults are backtick (\samp{`}) if the DBMS is identified
    as \code{"MySQL"} by the driver, and double quote otherwise.
#ifdef windows
    The Access, DBase and Excel wrappers set \code{tabQuote = c("[", "]")}.
#endif
    A user reported that the SAS ODBC driver required \code{colQuote = NULL}.
  }
  \item{interpretDot}{logical.  Should table names of the form
    \code{\var{qualifier}.\var{table}} be interpreted  as table
    \code{\var{table}} in schema \code{\var{qualifier}} (and for MySQL
    \sQuote{schema} means database)?}
  \item{DBMSencoding}{character string naming the encoding returned by
    the DBMS.  The default means the encoding of the locale \R is running
    under.  Values other than the default require \code{\link{iconv}} to 
    be available: it always is from \R 2.10.0, otherwise see
    \code{\link{capabilities}}.}
  \item{rows_at_time}{The default number of rows to fetch at a time,
    between 1 and 1024.  Not all drivers work correctly with values > 1: see
    \code{\link{sqlQuery}}.}
  \item{readOnlyOptimize}{logical: should the connection be optimized
    for read-only access?}
#ifdef windows
  \item{access.file, dbf.file, xls.file}{file of an appropriate type.}
  \item{readOnly}{logical: should the connection be read-only?}
#endif
}
\details{
  \code{odbcConnect} establishes a connection to the specified DSN, and
  \code{odbcDriverConnect} allows a more flexible specification \emph{via} a
  connection string.  \code{odbcConnect} uses the connection string\cr
  \code{"DSN=\var{dsn};UID=\var{uid};PWD=\var{pwd}"},\cr
  omitting the last two components if they are empty.
#ifdef windows
  See the examples for other uses of connection strings.

  Under the Windows GUI, specifying an incomplete \code{connection}, for
  example the default \code{""}, will bring up a dialog box to complete the
  information required.  (This does not work from \code{Rterm.exe}
  unless a driver is specified, a Windows restriction.)
#endif

  For DBMSs that translate table and column names \code{case} must
  be set appropriately.  Allowable values are \code{"nochange"},
  \code{"toupper"} and \code{"tolower"} as well as the names of
  databases where the behaviour is known to us (currently
  \code{"mysql"}, which maps to lower case on Windows but not on Linux,
  \code{"postgresql"} (lower), and \code{"msaccess"} (nochange)).  If
  \code{case} is not specified, the default is \code{"nochange"} unless
  the appropriate value can be figured out from the DBMS name reported
  by the ODBC driver.  It is likely that \code{"toupper"} is desirable
  on IBM's DB2, but this is not enforced.
#ifdef windows
  (The DBase driver is unusual: it preserves names on reading, but
  converts both table and column names to upper case on writing, and
  truncates table names to 8 characters.  RODBC does not attempt to do
  any mapping for that driver.)
#endif
  
  Note that \code{readOnlyOptimize} may do nothing, and is \strong{not}
  guaranteed to enforce read-only access.  With drivers that support it,
  it is used to optimize locking strategies, transaction management and
  so on.  It does make access to Mimer read-only, and has no effect on
  MySQL.
  
  Function \code{odbcReConnect} re-connects to a database using the
  settings of an existing (and presumably now closed) channel object.
  Arguments given in the original call can be overridden as needed.

  Note that if a password is supplied (either as a \code{pwd} argument
  or as part of the DSN) it may be stored in the
  \code{connection.string} element of the return value, but the value is
  (from \pkg{RODBC} 1.3-0) replaced by \code{******}.  (This will break
  \code{odbcReConnect}.)
#ifdef windows

  \code{odbcConnectAccess}, \code{odbcConnectDbase} and
  \code{odbcConnectExcel} are convenience wrappers to generate
  connection strings for those file types.  The files given can be
  relative to the \R working directory or absolute paths (and it seems
  also relative to the user's home directory). The file name can be
  omitted, which will on \code{Rgui} bring up a dialog box to search for
  a file.

  \bold{Note:} they will only work with English-language 32-bit versions
  of the Microsoft drivers, which may or may not be installed in other
  locales, and are not usable from 64-bit \R.  The \code{2007} versions
  work with the drivers which are installed with Office 2007/2010 and
  give access to formats such as \file{*.xlsx} and \file{*.accdb}.
  These drivers are also available separately and there is a 64-bit
  version: see the package manual.  (You must have the 32-bit drivers
  when using 32-bit \R and the 64-bit drivers when using 64-bit \R:
  otherwise there will be a cryptic message about a driver not being
  found.  And the 64-bit drivers cannot be installed alongside 32-bit
  Microsoft Office, and \emph{vice versa}.

  See the package manual for some of the peculiarities of the Excel
  drivers.  \code{readOnly = TRUE} may allow very limited changes (to
  insert and update rows).
#endif

  If it is possible to set the DBMS or ODBC driver to communicate in the
  character set of the \R session then this should be done.  For
  example, MySQL can set the communication character set \emph{via} SQL,
  e.g. \samp{SET NAMES 'utf8'}.
}

\note{
  Several errors which have been reported as bugs in \pkg{RODBC} 1.3-0
  which were in fact ODBC driver errors that can be circumvented by
  setting \code{rows_at_time = 1} (and the warning under that argument has
  always been there).  The drivers involved have been third-party
  Oracle drivers and old SQL Server drivers.
}

\value{
  A non-negative integer which is used as handle if no error occurred,
  \code{-1} otherwise.  A successful return has class \code{"RODBC"}, and
  attributes including
  \item{connection.string}{the full ODBC connection string.}
  \item{case}{the value of \code{case}.}
  \item{id}{a numeric ID for the channel.}
  \item{believeNRows}{the value of \code{believeNRows}.}
  \item{rows_at_time}{the value of \code{rows_at_time}.}
}
\author{
  Michael Lapsley, Brian Ripley
}
\seealso{
  \code{\link{odbcClose}}, \code{\link{sqlQuery}}, \code{\link{odbcGetInfo}}
}
\examples{\dontrun{
#ifdef unix
# MySQL
channel <- odbcConnect("test", uid="ripley", pwd="secret")
# PostgreSQL: 'case' should be detected automatically
channel <- odbcConnect("pg", uid="ripley", pwd="secret", case="postgresql")
#endif
#ifdef windows
# interactive specification under RGui
channel <- odbcDriverConnect("")

# MySQL on Windows -- MySQL maps to lower case on Windows only
channel <- odbcConnect("testdb", uid="ripley", case="tolower")

# Access
channel <- odbcConnect("testacc") # if this was set up as a DSN
channel2 <- odbcConnectAccess("test.mdb", uid="ripley")

# Excel
channel <- odbcConnect("bdr.xls") # if this was set up as a DSN
channel2 <-
 odbcDriverConnect(paste("DRIVER=Microsoft Excel Driver (*.xls)",
                         "DBQ=D:\\bdr\\hills.xls",
                         "ReadOnly=False", sep = ";"))
## or "DRIVER=Microsoft Excel Driver (*.xls *.xlsx, *.xlsm, *.xlsb)"
channel3 <- odbcConnectExcel("hills.xls")
#endif

# re-connection
odbcCloseAll()
channel <- odbcReConnect(channel) # must re-assign as the data may change
}}
\keyword{IO}
\keyword{database}
