% file RODBC/man/odbcSetAutoCommit.Rd
% copyright (C) 2005-2013  B. D. Ripley
%
\name{odbcSetAutoCommit}
\alias{odbcSetAutoCommit}
\alias{odbcEndTran}
\title{ODBC Set Auto-Commit Mode}
\description{
  Set ODBC database connection's auto-commit mode.
}
\usage{
odbcSetAutoCommit(channel, autoCommit = TRUE)

odbcEndTran(channel, commit = TRUE)
}
\arguments{
  \item{channel}{RODBC connection object returned by \code{odbcConnect}.}
  \item{autoCommit}{logical.  Set auto-commit on?}
  \item{commit}{logical.  Commit or rollback pending transaction?}
}
\details{
  Auto-commit is a concept supported only by ODBC connections to
  transactional DBMSs.
  
  If a connection to a transactional DBMS is in auto-commit mode (the
  default), then all its SQL statements will be executed and committed
  as individual transactions.  Otherwise, its SQL statements are grouped
  into transactions that are terminated by an execution of \code{commit}
  or \code{rollback}.  Switching a connection to auto-commit mode
  commits the pending transaction.

  By default, new connections are in auto-commit mode.  If auto-commit
  mode has been disabled, a call to \code{odbcEndTran} or an SQL
  \code{commit} statement must be executed in order to commit changes;
  otherwise, pending database changes will not be saved.
}
\value{
  \code{odbcSetAutoCommit} stops if \code{channel} is an invalid
  connection.  The function returns \code{-1} on error, \code{0} on
  success and \code{} on success with a message that would be returned
  by \code{odbcGetErrMsg}.
}
\author{
  Norman Yamada, Yasser El-Zein
}
\keyword{IO}
\keyword{database}
