% file RODBC/man/odbcDataSources.Rd
% copyright (C) 2006-2013  B. D. Ripley
%
\name{odbcDataSources}
\alias{odbcDataSources}
\title{List ODBC Data Sources}
\description{
  List known ODBC data sources.
}
\usage{
odbcDataSources(type = c("all", "user", "system"))
}
\arguments{
  \item{type}{User DSNs, system DSNs or all?}
}
\value{
  A named character vector of DSN descriptions, with names the DSNs.
}
\author{
  Brian Ripley
}
\examples{\dontrun{
> odbcDataSources()
        test      sqlite3       testpg 
     "MySQL"    "sqlite3" "PostgreSQL"
or
                          testdb3                           sqlite3
         "MySQL ODBC 3.51 Driver"             "SQLite3 ODBC Driver"
                          bdr.xls                           testacc
 "Microsoft Excel Driver (*.xls)" "Microsoft Access Driver (*.mdb)"
                           testpg                         SQLServer
                "PostgreSQL ANSI"               "SQL Native Client"
                           Oracle                               DB2
       "Oracle in OraDb10g_home1"  "IBM DB2 ODBC DRIVER - DB2COPY1"
                          testpgw                           testdb5
             "PostgreSQL Unicode"           "MySQL ODBC 5.1 Driver"
                SQLite Datasource           SQLite UTF-8 Datasource
             "SQLite ODBC Driver"      "SQLite ODBC (UTF-8) Driver"
               SQLite3 Datasource                             mimdb
            "SQLite3 ODBC Driver"                           "MIMER"
}}
\keyword{utilities}