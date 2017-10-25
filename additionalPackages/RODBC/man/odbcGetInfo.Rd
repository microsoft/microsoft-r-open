% file RODBC/man/odbcGetInfo.Rd
% copyright (C) 1999-2002  M. Lapsley
% copyright (C) 2002-2013  B. D. Ripley
%
\name{odbcGetInfo}
\alias{odbcGetInfo}
\title{Request Information on an ODBC Connection}
\description{
  Request information on an ODBC connection.
}
\usage{
odbcGetInfo(channel)
}
\arguments{
  \item{channel}{connection handle as returned by
    \code{\link{odbcConnect}} of class \code{"RODBC"}.}
}
\value{
  A named character string giving information on the database and ODBC driver
  in use on the connection \code{channel}.
}
\author{
  Brian Ripley
}
\examples{\dontrun{
odbcGetInfo(channel) # under Windows XP
## MySQL returned
             DBMS_Name               DBMS_Ver        Driver_ODBC_Ver
               "MySQL"     "5.1.35-community"                "03.51"
      Data_Source_Name            Driver_Name             Driver_Ver
             "testdb5"          "myodbc5.dll"           "05.01.0005"
              ODBC_Ver            Server_Name
          "03.52.0000" "localhost via TCP/IP"
## MS Access returned
       DBMS_Name         DBMS_Ver  Driver_ODBC_Ver Data_Source_Name
        "ACCESS"     "04.00.0000"          "03.51"        "testacc"
     Driver_Name       Driver_Ver         ODBC_Ver      Server_Name
  "odbcjt32.dll"     "04.00.6305"     "03.52.0000"         "ACCESS"
## SQL Server 2008 Express returned
             DBMS_Name               DBMS_Ver        Driver_ODBC_Ver
"Microsoft SQL Server"           "10.00.1600"                "03.52"
      Data_Source_Name            Driver_Name             Driver_Ver
           "SQLServer"          "SQLNCLI.DLL"           "09.00.4035"
              ODBC_Ver            Server_Name
          "03.52.0000"      "AUK\\\\SQLEXPRESS"
}}
\keyword{IO}
\keyword{database}
