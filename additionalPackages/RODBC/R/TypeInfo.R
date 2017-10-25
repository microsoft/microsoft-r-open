# file RODBC/R/TypeInfo.R
# copyright (C) 2004-2011  B. D. Ripley
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

sqlTypeInfo <-
    function(channel, type = "all", errors = TRUE, as.is = TRUE)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    type <- match(type, c("all", "char", "varchar", "real",
                         "double", "integer", "smallint", "timestamp",
                          "float", "bit", "wchar", "wvarchar",
                          "date", "time", "binary", "varbinary",
                          "longvarbinary", "blob"),
                  nomatch = 1L) - 1L
    stat <- .Call(C_RODBCTypeInfo, attr(channel, "handle_ptr"),
                  as.integer(type), PACKAGE = "RODBC")
    if(!stat) {
        if(errors) return(odbcGetErrMsg(channel)) else return(invisible(-1L))
    } else {
        ## Several MySQL drivers failed to set the number of rows
        ## correctly here, including 3.51.26 and 5.1.5
        return(sqlGetResults(channel, as.is = as.is, errors = errors,
                             believeNRows = FALSE))
    }
}

typesR2DBMS <-
    list2env(list(
                  MySQL = list(double="double", integer="integer",
                  character="varchar(255)", logical="varchar(5)"),
                  ACCESS = list(double="DOUBLE", integer="INTEGER",
                  character="VARCHAR(255)", logical="varchar(5)"),
                  ## float is double, real is single
                  "Microsoft SQL Server" = list(double="float", integer="int",
                  character="varchar(255)", logical="varchar(5)"),
                  PostgreSQL = list(double="float8", integer="int4",
                  character="varchar(255)", logical="varchar(5)"),
                  ## was double="double precision", integer="integer"
                  Oracle = list(double="binary_double", integer="decimal",
                  character="varchar(255)", logical="varchar(5)"),
                  SQLite = list(double="double", integer="integer",
                  character="varchar(255)", logical="varchar(5)"),
                  EXCEL = list(double="NUMBER", integer="NUMBER",
                  character="VARCHAR(255)", logical="LOGICAL"),
                  DBASE = list(double="Numeric", integer="Numeric",
                  character="Char(254)", logical="Logical"),
                  "DB2/NT" = list(double="DOUBLE", integer="INTEGER",
                  character="VARCHAR(255)", logical="VARCHAR(5)"),
                  "Mimer SQL Engine" = list(double="DOUBLE PRECISION",
                  integer="INTEGER", character="VARCHAR(255)",
                  logical="VARCHAR(5)")
                  ))

getSqlTypeInfo <- function(driver)
{
    if(missing(driver)) {
        res <- t(data.frame(lapply(typesR2DBMS, as.character),
                            check.names=FALSE))
        colnames(res) <- c("double", "integer", "character", "logical")
        as.data.frame(res)
    } else typesR2DBMS[[driver]]
}

setSqlTypeInfo <- function(driver, value)
{
    if(!is.character(driver) || length(driver) != 1L)
        stop("argument 'driver' must be a character string")
    if(!is.list(value) || length(value) < 4L || is.null(names(value)) )
        stop("argument 'value' must be a named list of length >= 4")
    typesR2DBMS[[driver]] <<- value[c("double", "integer", "character", "logical")]
}
