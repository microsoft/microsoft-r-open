# file RODBC/R/sql.R
# copyright (C) 1999-2002  M. Lapsley
# copyright (C) 2002-2009  B. D. Ripley
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
# high level functions for sql database access
#
###########################################

sqlClear <- function(channel, sqtable, errors = TRUE)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    if(missing(sqtable)) stop("missing argument 'sqtable'")
    dbname <- odbcTableExists(channel, sqtable, abort = errors)
    if(!length(dbname)) {
        if(errors) stop("table ", sQuote(sqtable), " not found");
        return(invisible(-1L));
    }
    res <- sqlQuery(channel, paste ("TRUNCATE TABLE", dbname), errors = errors)
##    res <- sqlQuery(channel, paste ("DELETE FROM", dbname), errors = errors)
    if(errors &&
       (!length(res) || identical(res, "No Data") )) invisible()
    else invisible(res)
}

sqlDrop <- function(channel, sqtable, errors = TRUE)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    if(missing(sqtable)) stop("missing argument 'sqtable'")
    dbname <- odbcTableExists(channel, sqtable, abort = errors)
    if(!length(dbname)) {
        if(errors) stop("table ", sQuote(sqtable), " not found");
        return(invisible(-1L));
    }
    res <- sqlQuery(channel, paste ("DROP TABLE", dbname), errors = errors)
    ## Windows and SQLite were returning character(0)
    if(errors &&
       (!length(res) || identical(res, "No Data") )) invisible()
    else invisible(res)
}


sqlFetch <-
    function (channel, sqtable, ..., colnames = FALSE, rownames = TRUE)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    if(missing(sqtable)) stop("missing argument 'sqtable'")
    dbname <- odbcTableExists(channel, sqtable)
    ans <- sqlQuery(channel, paste("SELECT * FROM", dbname), ...)
    if(is.data.frame(ans)) {
        if(is.logical(colnames) && colnames) {
            colnames(ans) <- as.character(as.matrix(ans[1L, ]))
            ans <- ans[-1L, ]
        }
        ## FIXME case-mangling?
        if(is.logical(rownames) && rownames) rownames <- "rownames"
        if(is.character(rownames)) {
            cn <- names(ans)
            if (!is.na(rn <- match(rownames, cn))) {
                row.names(ans) <- as.character(ans[, rn])
                ans <- ans[, -rn, drop = FALSE]
            }
        }
    }
    ans
}

sqlFetchMore <-
    function (channel, ..., colnames = FALSE, rownames = TRUE)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    ans <- sqlGetResults(channel, ...)
    if(is.data.frame(ans)) {
        if(is.logical(colnames) && colnames) {
            colnames(ans) <- as.character(as.matrix(ans[1L, ]))
            ans <- ans[-1L, ]
        }
        ## FIXME case-mangling?
        if(is.logical(rownames) && rownames) rownames <- "rownames"
        if(is.character(rownames)) {
            cn <- names(ans)
            if (!is.na(rn <- match(rownames, cn))) {
                row.names(ans) <- as.character(ans[, rn])
                ans <- ans[, -rn, drop = FALSE]
            }
        }
    }
    ans
}

sqlCopy <-
    function(channel, query, destination, destchannel = channel,
             verbose = FALSE, errors = TRUE, ...)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    if(!odbcValidChannel(destchannel))
       stop("destination argument is not an open RODBC channel")
    if( missing(query) || missing(destination))
        stop("missing parameter")
    if(length(destination) != 1L)
        stop("destination should be a name")
    dataset <- sqlQuery(channel, query, errors = errors)
    sqlSave(destchannel, dataset, destination, verbose=verbose, ...)
}

sqlCopyTable <-
    function (channel, srctable, desttable, destchannel = channel,
              verbose = FALSE, errors = TRUE)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    if(!odbcValidChannel(destchannel))
       stop("destination argument is not an open RODBC channel")
    if(missing(srctable) || missing(desttable))
        stop("missing parameter")
    dtablename <- as.character(desttable)
    if(length(dtablename) != 1L)
        stop(sQuote(dtablename), " should be a name")
    stablename <- as.character(srctable)
    if(!length(odbcTableExists(channel, stablename, abort = errors)))
        return(invisible(-1L));
    query <- sqltablecreate(channel, dtablename,
                            coldata = sqlColumns(channel, stablename),
                            keys = sqlPrimaryKeys(channel, stablename))
    if(verbose) cat("Query: ", query, "\n", sep = "")
    sqlQuery(destchannel, query, errors=errors)
}



######################################################
# 	sqlSave
#	save into table if exists
#	if table not compatible delete it
#	create new table.


sqlSave <-
    function(channel, dat, tablename = NULL, append = FALSE, rownames = TRUE,
             colnames = FALSE, verbose = FALSE,
             safer = TRUE, addPK = FALSE, typeInfo, varTypes,
             fast = TRUE, test = FALSE, nastring = NULL)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    if(missing(dat))
        stop("missing parameter")
    if(!is.data.frame(dat))
        stop("should be a data frame")
    if(is.null(tablename))
        tablename <- if(length(substitute(dat)) == 1)
            as.character(substitute(dat))
        else
            as.character(substitute(dat)[[2L]])
    if(length(tablename) != 1L)
        stop(sQuote(tablename), " should be a name")
    switch(attr(channel, "case"),
           nochange = {},
           toupper={tablename <- toupper(tablename)
                    colnames(dat) <- toupper(colnames(dat))},
           tolower={tablename <- tolower(tablename)
                    colnames(dat) <- tolower(colnames(dat))}
           )

    keys <- -1
    ## move row labels into data frame
    ## FIXME case-mangling?
    if(is.logical(rownames) && rownames) rownames <- "rownames"
    if(is.character(rownames)) {
        dat <- cbind(row.names(dat), dat)
        names(dat)[1L] <- rownames
        if(addPK) {
            keys <- vector("list", 4L)
            keys[[4L]] <- rownames
        }
    }
    ## ? move col names into dataframe (needed to preserve case)
    if(is.logical(colnames) && colnames) {
        ## this is to deal with type conversions
        as.data.frame(rbind(colnames(dat), as.matrix(dat)))->dat
    }
    ## find out if table already exists
    dbname <- odbcTableExists(channel, tablename, abort = FALSE)
    if(length(dbname)) {
        if(!append) {
            if(safer) stop("table ", sQuote(tablename), " already exists")
            ## zero table, return if no perms
            query <- paste ("DELETE FROM", dbname)
            if(verbose) cat("Query: ", query, "\n", sep = "")
            res <- sqlQuery(channel, query, errors = FALSE)
            if(is.numeric(res) && res == -1L) # No Data is fine
                stop(paste(odbcGetErrMsg(channel), collapse="\n"))
        }
        if(sqlwrite(channel, tablename, dat, verbose=verbose, fast=fast,
                    test=test, nastring=nastring) == -1) {
            ##cannot write: try dropping table
            query <- paste("DROP TABLE", dbname)
            if(verbose) {
                cat("sqlwrite returned ", odbcGetErrMsg(channel),
                    "\n", sep = "\n")
                cat("Query: ", query, "\n", sep = "")
            }
            if(safer) stop("unable to append to table ", sQuote(tablename))
            res <- sqlQuery(channel, query, errors = FALSE)
            if(is.numeric(res) && res == -1L) # No Data is fine
                stop(paste(odbcGetErrMsg(channel), collapse="\n"))
        } else { #success
            return (invisible(1L))
        }
    }
#  we get here if:
#  -	no table
#  -	table with invalid columns
#	No permissions for existing table should have aborted above

    types <- sapply(dat, typeof)
    facs <- sapply(dat, is.factor)
    isreal <- (types == "double")
    isint <- (types == "integer") & !facs
    islogi <- (types == "logical")
    colspecs <- rep("varchar(255)", length(dat))
    if(!missing(typeInfo) ||
       !is.null(typeInfo <- typesR2DBMS[[odbcGetInfo(channel)[1L]]])) {
        colspecs <- rep(typeInfo$character[1L], length(dat))
        colspecs[isreal] <- typeInfo$double[1L]
        colspecs[isint] <- typeInfo$integer[1L]
        colspecs[islogi] <- typeInfo$logical[1L]
    } else {
        typeinfo <- sqlTypeInfo(channel, "all", errors = FALSE)
        if(is.data.frame(typeinfo)) {
            ## Now change types as appropriate.
            if(any(isreal)) {
                realinfo <- sqlTypeInfo(channel, "double")[, 1L]
                if(length(realinfo) > 0L) {
                    if(length(realinfo) > 1L) { # more than one match
                        nm <- match("double", tolower(realinfo))
                        if(!is.na(nm)) realinfo <- realinfo[nm]
                    }
                    colspecs[isreal] <- realinfo[1L]
                } else {
                    realinfo <- sqlTypeInfo(channel, "float")[, 1L]
                    if(length(realinfo) > 0L) {
                        if(length(realinfo) > 1L) { # more than one match
                            nm <- match("float", tolower(realinfo))
                            if(!is.na(nm)) realinfo <- realinfo[nm]
                        }
                        colspecs[isreal] <- realinfo[1L]
                    }
                }
            }
            if(any(isint)) {
                intinfo <- sqlTypeInfo(channel, "integer")[, 1L]
                if(length(intinfo) > 0L) {
                    if(length(intinfo) > 1) { # more than one match
                        nm <- match("integer", tolower(intinfo))
                        if(!is.na(nm)) intinfo <- intinfo[nm]
                    }
                    colspecs[isint] <- intinfo[1L]
                }
            }
        }
    }
    names(colspecs) <- names(dat)
    if(!missing(varTypes)) {
        if(!length(nm <- names(varTypes)))
            warning("argument 'varTypes' has no names and will be ignored")
        OK <- names(colspecs) %in% nm
        colspecs[OK] <- varTypes[names(colspecs)[OK]]
        notOK <- !(nm %in% names(colspecs))
        if(any(notOK))
            warning("column(s) ", paste(nm[notOK], collapse=", "),
                    " 'dat' are not in the names of 'varTypes'")
    }
    ## CREATE TABLE does not allow sheet names, so cannot make an
    ## exception for Excel here
    query <- sqltablecreate(channel, tablename, colspecs = colspecs,
                            keys = keys)
    if(verbose) cat("Query: ", query, "\n", sep = "")
    ##last chance:  let it die if fails
    res <- sqlQuery(channel, query, errors = FALSE)
    if(is.numeric(res) && res == -1) # No Data is fine
        stop(paste(odbcGetErrMsg(channel), collapse="\n"))
    if(sqlwrite(channel, tablename, dat, verbose=verbose, fast=fast,
                test=test, nastring=nastring) < 0) {
        err <- odbcGetErrMsg(channel)
        msg <- paste(err,  collapse="\n")
        if("missing column name" %in% err)
            msg <- paste(msg,
                         "Check case conversion parameter in odbcConnect",
                         sep="\n")
        stop(msg)
    }
    invisible(1L)
}

mangleColNames <- function(colnames) gsub("[^[:alnum:]_]+", "", colnames)

quoteColNames <- function(channel, colnames)
{
    quotes <- attr(channel, "colQuote")
    if(length(quotes) >= 2L)
        paste(quotes[1L], colnames, quotes[2L], sep="")
    else if(length(quotes) == 1L)
        paste(quotes, colnames, quotes, sep="")
    else colnames
}

quoteTabNames <- function(channel, tablename)
{
    if(attr(channel, "interpretDot") &&
       grepl(".", tablename, fixed = TRUE)) return(tablename)
    quotes <- attr(channel, "tabQuote")
    if(length(quotes) >= 2L)
        paste(quotes[1L], tablename, quotes[2L], sep="")
    else if(length(quotes) == 1L)
        paste(quotes, tablename, quotes, sep="")
    else tablename
}

################################################
# utility function
# write to table with name data

##############################################

sqlwrite <-
    function (channel, tablename, mydata, test = FALSE, fast = TRUE,
              nastring = NULL, verbose = FALSE)
{
    if(!odbcValidChannel(channel))
        stop("first argument is not an open RODBC channel")
    colnames <- as.character(sqlColumns(channel, tablename)[4L][, 1L])
    ## match the transform in tablecreate (get rid of invalid chars in col names)
    colnames <- mangleColNames(colnames)
    cnames <- paste(quoteColNames(channel, colnames), collapse = ", ")
    dbname <- quoteTabNames(channel, tablename)
    if(!fast) {
        ## this doesn't do a good enough job with logicals.
        for(i in seq_along(mydata))
            if(is.logical(mydata[[i]])) mydata[[i]] <- as.character(mydata[[i]])
        data <- as.matrix(mydata)
        if(nchar(enc<- attr(channel, "encoding")) && is.character(data))
            data <- iconv(data, to = enc)
        colnames(data) <- colnames
        ## quote character and date columns
        cdata <- sub("\\([[:digit:]]*\\)", "",
                     sqlColumns(channel, tablename)[, "DATA_TYPE"])
        tdata <- sqlTypeInfo(channel)
        nr <- match(cdata, tdata[, 2L])
        tdata <- as.matrix(tdata[nr, 4:5])
        ## quote all unkown types.
        tdata[is.na(nr), ] <- "'"
        for(cn in seq_along(cdata)) {
            td <- as.vector(tdata[cn,])
            if(is.na(td[1L])) next
            if(identical(td, rep("'", 2L)))
               data[, cn] <- gsub("'", "''", data[, cn])
            data[, cn] <- paste(td[1L], data[, cn], td[2L], sep = "")
        }
        data[is.na(mydata)] <- if(is.null(nastring)) "NULL" else nastring[1L]
        for (i in 1L:nrow(data)) {
            query <- paste("INSERT INTO", dbname, "(", cnames,
                           ") VALUES (",
                           paste(data[i, colnames], collapse = ", "),
                           ")")
            if(verbose) cat("Query: ", query, "\n", sep = "")
            if(odbcQuery(channel, query) < 0) return(-1L)
        }
    } else {
        query <- paste("INSERT INTO", dbname, "(", cnames, ") VALUES (",
                       paste(rep("?", ncol(mydata)), collapse=","), ")")
        if(verbose) cat("Query: ", query, "\n", sep = "")
	coldata <- sqlColumns(channel, tablename)[c(4L,5L,7L,9L)]
        if(any(is.na(m <- match(colnames, coldata[, 1])))) return(-1L)
        ## sometimes drivers get this wrong e.g. sqliteodbc
        if(any(notOK <- (coldata[,3L] == 0L))) {
            types <- coldata[notOK, 2]
            tdata <- sqlTypeInfo(channel)
            coldata[notOK, 3L] <- tdata[match(types, tdata[, 2L]), 3L]
        }
        if(odbcUpdate(channel, query, mydata, coldata[m, ],
                      test = test, verbose = verbose,
                      nastring = nastring) < 0) return(-1L)
    }
    return(invisible(1L))
}


#
#       Generate create statement
#	parameter coldata is output from sqlColumns
#	parameter keys is output from sqlPrimaryKeys (and might be -1)
#	NB: some systems do not support sqlPKs
##############################################

sqltablecreate <-
    function (channel, tablename, coldata = NULL, colspecs, keys = -1)
{
    create <- paste("CREATE TABLE", quoteTabNames(channel, tablename), " (")
    if(!is.null(coldata)) { # called from sqlCopyTable
        j <- nrow(coldata)
        colnames <- as.character(coldata[, 4L])
        for (i in 1L:j) {
            ## 4 =rowname, 6 coltype, 7 col size, 11 ? nullable
            if(coldata[i, 11] == 1) {
                null <- " NULL"
                null <- ""  # Kludge for Oracle till bug fixed
            } else {
                null <- " NOT NULL"
            }
            colsize <- if(coldata[i, 7L] == 65535) " " else paste("(", coldata[i,7], ") ", sep="")
            create <- paste(create,
                            quoteColNames(channel, mangleColNames(colnames[i])),
                            " ", coldata[i, 6L], colsize, null, sep="")
            if(!is.numeric(keys)) {
                if(as.character(keys[[4L]]) == colnames[i])
                    create <- paste(create, "PRIMARY KEY")
            }
            if(i < j) create <- paste(create, ", ")
        }
    } else { # called from sqlSave
        colnames <- quoteColNames(channel, mangleColNames(names(colspecs)))
        entries <- paste(colnames, colspecs)
        if(is.list(keys)) {
            keyname <- as.character(keys[[4L]])
            key <- match(keyname, names(colspecs))
            entries[key] <- paste(entries[key], "NOT NULL PRIMARY KEY")
        }
        create <- paste(create, paste(entries, collapse = ", "), sep="")
    }
    create <- paste(create, ")", sep="")
    create
}

###############################################
#
#####  Query Functions
#	Return a data frame according to as.is
#
###############################################

sqlTables <- function(channel, errors = FALSE, as.is = TRUE,
                      catalog = NULL, schema = NULL,
                      tableName = NULL, tableType = NULL, literal = FALSE)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    stat <- odbcTables(channel, catalog = catalog, schema = schema,
                       tableName = tableName, tableType = tableType,
                       literal = literal)
    if(stat < 0L) {
        if(errors) {
            if(stat == -2L) stop("invalid channel")
            else return(odbcGetErrMsg(channel))
        } else return(invisible(-1L))
    } else return(sqlGetResults(channel, as.is = as.is))
}

sqlColumns <-
    function (channel, sqtable, errors = FALSE, as.is = TRUE, special = FALSE,
              catalog = NULL, schema = NULL, literal = FALSE)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    if(length(sqtable) != 1)
        stop(sQuote(sqtable), " should be a name")
    if(is.null(catalog) && is.null(schema)) {
        dbname <- odbcTableExists(channel, sqtable, FALSE, FALSE)
        if(!length(dbname)) {
            caseprob <- ""
            if(is.data.frame(nm <- sqlTables(channel)) &&
               tolower(sqtable) %in% tolower(nm[,3]))
                caseprob <-  "\nCheck case parameter in odbcConnect"
            stop(sQuote(sqtable), ": table not found on channel", caseprob)
        }
        if(grepl(".", dbname, fixed = TRUE)) {
            parts <- strsplit(dbname, ".", fixed = TRUE)[[1]]
            if(length(parts) > 2) stop("dot.dot.dot names are not supported")
            if(attr(channel, "isMySQL")) {
                ## This does not work for current drivers
                catalog <- parts[1]
                dbname <- parts[2]
            } else {
                schema <- parts[1]
                dbname <- parts[2]
            }
        }
    } else
        dbname <- switch(attr(channel, "case"),
                         nochange = sqtable, toupper = toupper(sqtable),
                         tolower = tolower(sqtable))
    stat <- if(special)
        odbcSpecialColumns(channel, dbname, catalog, schema)
    else odbcColumns(channel, dbname, catalog, schema, literal)
    if(stat < 0L) {
        if(errors) {
            if(stat == -2L) stop("invalid channel")
            else return(odbcGetErrMsg(channel))
        } else return(invisible(-1L))
    } else return(sqlGetResults(channel, as.is = as.is))
}

sqlPrimaryKeys <-
    function(channel, sqtable, errors = FALSE, as.is = TRUE,
             catalog = NULL, schema = NULL)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    if(length(sqtable) != 1)
        stop(sQuote(sqtable), " should be a name")
    if(is.null(catalog) && is.null(schema)) {
        dbname <- odbcTableExists(channel, sqtable, FALSE, FALSE)
        if(!length(dbname)) {
            caseprob <- ""
            if(is.data.frame(nm <- sqlTables(channel)) &&
               tolower(sqtable) %in% tolower(nm[, 3L]))
                caseprob <-  "\nCheck case parameter in odbcConnect"
            stop(sQuote(sqtable), ": table not found on channel", caseprob)
        }
        if(grepl(".", dbname, fixed = TRUE)) {
            parts <- strsplit(dbname, ".", fixed = TRUE)[[1]]
            if(length(parts) > 2) stop("dot.dot.dot names are not supported")
            if(attr(channel, "isMySQL")) {
                ## This does not work for current drivers
                catalog <- parts[1]
                dbname <- parts[2]
            } else {
                schema <- parts[1]
                dbname <- parts[2]
            }
        }
    } else
        dbname <- switch(attr(channel, "case"),
                         nochange = sqtable, toupper = toupper(sqtable),
                         tolower = tolower(sqtable))
    stat <- odbcPrimaryKeys(channel, dbname, catalog, schema)
    if(stat < 0L) {
        if(errors) {
            if(stat == -2L) stop("invalid channel")
            else return(odbcGetErrMsg(channel))
        } else return(invisible(-1L))
    } else return(sqlGetResults(channel, as.is = as.is))
}

sqlQuery <-
    function(channel, query, errors = TRUE, ..., rows_at_time)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    if(missing(query))
        stop("missing argument 'query'")
    ## could argue that 'max' should restrict rows_at_time
    rows_at_time <- if(missing(rows_at_time)) attr(channel, "rows_at_time")
    else max(1, min(1024, rows_at_time))
    stat <- odbcQuery(channel, query, rows_at_time)
    if(stat == -1L) {
        if(errors) return(odbcGetErrMsg(channel))
        else return(invisible(stat))
    } else return(sqlGetResults(channel, errors = errors, ...))
}


sqlGetResults <-
    function (channel, as.is = FALSE,
              errors = FALSE, max = 0, buffsize = 1000,
              nullstring = NA_character_, na.strings = "NA",
              believeNRows = TRUE, dec = getOption("dec"),
              stringsAsFactors = default.stringsAsFactors())
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    as.df <- function(value, colnames) {
        for(i in seq_along(value))
            if(is.list(value[[i]])) class(value[[i]]) <- "ODBC_binary"
        ## convert list to data frame
        class(value) <- "data.frame"
        names(value) <- make.unique(colnames)
        row.names(value) <- seq(along=value[[1L]])
        value
    }
    cols <- .Call(C_RODBCNumCols, attr(channel, "handle_ptr"))
    ## FIXME: should this be <= 0L?
    if(cols < 0L) {
        if(errors) return("No data")
        else return(invisible(-1L))
    }

    cData <- .Call(C_RODBCColData, attr(channel, "handle_ptr"))

    dbdata <- odbcFetchRows(channel,
                            max = max,
                            buffsize = buffsize,
                            nullstring = nullstring,
                            believeNRows = believeNRows)
    if(dbdata$stat < 0L) {
	if(errors) return(odbcGetErrMsg(channel))
	else return(invisible(dbdata$stat))
    }

    data <- as.df(dbdata$data, cData$names)
    if(nrow(data) > 0L) {
        cols <- ncol(data)
        enc <- attr(channel, "encoding")
        if(length(na.strings))
            for (i in 1L:cols)
                if(is.character(data[,i]))
                    data[data[,i] %in% na.strings, i] <- NA
        if(is.logical(as.is)) {
            as.is <- rep(as.is, length = cols)
        } else if(is.numeric(as.is)) {
            if(any(as.is < 1 | as.is > cols))
                stop("invalid numeric 'as.is' expression")
            i <- rep(FALSE, cols)
            i[as.is] <- TRUE
            as.is <- i
        } else if(length(as.is) != cols)
            stop("'as.is' has the wrong length ", length(as.is),
                 " != cols = ", cols)
        for (i in seq_len(cols)) {
            if(is.character(data[[i]]) && nchar(enc))
                data[[i]] <- iconv(data[[i]], from = enc)
            if(as.is[i] || is.list(data[[i]])) next
            if(is.numeric(data[[i]])) next
            if(cData$type[i] == "date")
                data[[i]] <- as.Date(data[[i]])
            else if(cData$type[i] == "timestamp")
                data[[i]] <- as.POSIXct(data[[i]])
            else
                data[[i]] <- type.convert(as.character(data[[i]]),
                                          na.strings = na.strings,
                                          as.is = !stringsAsFactors,
                                          dec = dec)
        }
    }
    data
}

format.ODBC_binary <- print.ODBC_binary <- function(x, ...)
    sapply(x, function(x) paste(as.character(x), collapse=""))


#################################################

sqlUpdate <-
    function(channel, dat, tablename = NULL, index = NULL,
             verbose = FALSE, test = FALSE,
             nastring = NULL, fast = TRUE)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    if(missing(dat)) stop("missing parameter")
    if(!is.data.frame(dat)) stop("should be a data frame or matrix")
    if(is.null(tablename))
        tablename <- if(length(substitute(dat)) == 1L)
            as.character(substitute(dat))
        else
            as.character(substitute(dat)[[2L]])
    if(length(tablename) != 1L)
        stop(sQuote(tablename), " should be a name")
    dbname <- odbcTableExists(channel, tablename)
    ## test for missing values.
    cnames <- colnames(dat)
    ## match the transform in tablecreate (get rid of inval chars in col names)
    cnames <- mangleColNames(cnames)
    cnames <- switch(attr(channel, "case"),
                     nochange = cnames,
                     toupper = toupper(cnames),
                     tolower = tolower(cnames))
    ## get the column descriptor data for the rest of the table.
    ## This may or may not include the unique column depending
    ## on whether or not it is a special column.
    cdata <- sqlColumns(channel,tablename)
    coldata <- cdata[c(4L,5L,7L,9L)]
    if(is.character(index)) {
        intable <- index %in% coldata[ ,1L]
        if(any(!intable)) stop("index column(s) ",
                               paste(index[!intable], collapse=" "),
                               " not in database table")
        intable <- index %in% cnames
        if(any(!intable)) stop("index column(s) ",
                               paste(index[!intable], collapse=" "),
                               " not in data frame")
        indexcols <- index
    } else {
        haveKey <- FALSE
        ## identify the column(s) that is a unique row specifier along with
        ## its descriptor data.  First try a primary key
        indexcols <- sqlPrimaryKeys(channel, tablename)
        if(!(is.numeric(indexcols) || nrow(indexcols) == 0L)) {
            ## have primary key(s)
            index <- as.character(indexcols[, 4L])
            intable <- index %in% cnames
            if(any(intable)) {
                indexcols <- index[intable][1L]
                haveKey <- TRUE
            }
        }
        if(!haveKey){
            ## try special columns
            indexcols <- sqlColumns(channel, tablename, special = TRUE)
            if(!(is.numeric(indexcols) || nrow(indexcols) == 0L)) {
                indexcols <- indexcols[c(2L,3L,5L,7L)]

                ## check that the unique column(s) are present in the dataframe
                indexflags <- indexcols[, 1L] %in% cnames
                if(all(indexflags)) {
                ## if a unique column is not in coldata bind it on
                    incoldata <- indexcols[, 1L] %in% coldata[, 1L]
                    if(any(!incoldata))
                        coldata <- rbind(coldata, indexcols[!incoldata])
                    indexcols <- as.character(indexcols[, 1L])
                    haveKey <- TRUE
                }
            }
        }
        if(!haveKey){
            ## can we use a column 'rownames' as index column?
            m <- match("rownames", tolower(coldata[, 1L]))
            if(is.na(m))
                stop("cannot update ", sQuote(tablename),
                     " without unique column")
            indexcols <- coldata[m, 1L]
            dat <- cbind(row.names(dat), dat)
            names(dat)[1L] <- indexcols
            cnames <- c(indexcols, cnames)
        }
    }
    ## check that no columns are present in the df that are not in the table
    intable <- cnames %in% coldata[, 1L]
    if(any(!intable)) stop("data frame column(s) ",
                           paste(cnames[!intable], collapse=" "),
                           " not in database table")
    cn1 <- cnames[!cnames %in% indexcols]
    cn2 <- quoteColNames(channel, cn1)
    if(fast) {
        query <- paste("UPDATE", dbname, "SET")
        query <- paste(query,
                       paste(paste(cn2, "=?", sep =""), collapse = ", "))
        paramnames <- c(cn1, indexcols)
        if (length(indexcols)) {
            ind <- quoteColNames(channel, indexcols)
            query <- paste(query, "WHERE",
                           paste(paste(ind, "=?", sep =""),
                                 collapse = " AND "))
        }
        row.names(coldata) <- coldata[, 1L]
        paramdata <- coldata[paramnames, ]
        if(test | verbose) cat("Query: ", query, "\n", sep = "")
        stat <- odbcUpdate(channel, query, dat, paramdata, test = test,
                           verbose = verbose, nastring = nastring)
    } else {
        data <- as.matrix(dat)
        if(nchar(enc <- attr(channel, "encoding")) && is.character(data))
            data[] <- iconv(data, to = enc)
        ## we might have mangled and case-folded names on the database.
        colnames(data) <- cnames
        ## quote character etc columns
        cdata <- sub("\\([[:digit:]]*\\)", "",
                     sqlColumns(channel, tablename)[, "TYPE_NAME"])
        tdata <- sqlTypeInfo(channel)
        tdata <- as.matrix(tdata[match(cdata, tdata[, 1]), c(4,5)])
        for(cn in seq_along(cdata)) {
            td <- as.vector(tdata[cn,])
            if(is.na(td[1L])) next
            if(identical(td, rep("'", 2L)))
               data[, cn] <- gsub("'", "''", data[, cn])
            data[, cn] <- paste(td[1L], data[, cn], td[2L], sep = "")
        }
        data[is.na(dat)] <- if(is.null(nastring)) "NULL" else nastring
        for (i in 1L:nrow(data)) {
            query <- paste("UPDATE", dbname, "SET")
            query <- paste(query,
                           paste(paste(cn2, "=", data[i, cn1], sep =""),
                                 collapse = ", "))
            if (length(indexcols)) { # will always be true.
                ind <- quoteColNames(channel, indexcols)
                query <- paste(query, "WHERE",
                               paste(paste(ind, "=", data[i, indexcols], sep =""),
                                     collapse = " AND "))
            }
            if(verbose) cat("Query: ", query, "\n", sep = "")
            if((stat <- odbcQuery(channel, query)) < 0L) break
        }
    }
    if(stat < 0L) stop(paste(odbcGetErrMsg(channel), sep="\n"))
    invisible(stat)
}

odbcTableExists <- function(channel, tablename, abort = TRUE, forQuery = TRUE,
                            allowDot = attr(channel, "interpretDot"))
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    if(length(tablename) != 1)
        stop(sQuote(tablename), " should be a name")
    tablename <- as.character(tablename)
    switch(attr(channel, "case"),
           nochange = {},
           toupper = tablename <- toupper(tablename),
           tolower = tablename <- tolower(tablename)
           )
    isExcel <- odbcGetInfo(channel)[1L] == "EXCEL"
    hasDot <- grepl(".", tablename, fixed = TRUE)
    if(allowDot && hasDot) {
        parts <- strsplit(tablename, ".", fixed = TRUE)[[1]]
        ## FIXME
        if(length(parts) > 2) ans <- FALSE
        else {
            res <- if(attr(channel, "isMySQL"))
                sqlTables(channel, catalog = parts[1], tableName = parts[2])
            else
                sqlTables(channel, schema = parts[1], tableName = parts[2])
            ans <- is.data.frame(res) && nrow(res) > 0
        }
    } else if(!isExcel) {
        ## just ask about this name
        res <- sqlTables(channel, tableName = tablename)
        ans <- is.data.frame(res) && nrow(res) > 0
    } else {
        res <- sqlTables(channel)
        tables <- stables <- if(is.data.frame(res)) res[, 3] else ""
        ## Excel appends a $ to worksheets, single-quotes non-standard names
        if(isExcel) {
            tables <- sub("^'(.*)'$", "\\1", tables)
            tables <- unique(c(tables, sub("\\$$", "", tables)))
        }
        ans <- tablename %in% tables
    }
    if(abort && !ans)
        stop(sQuote(tablename), ": table not found on channel")

    enc <- attr(channel, "encoding")
    if(nchar(enc)) tablename <- iconv(tablename, to = enc)

    if(ans && isExcel) {
        dbname <- if(tablename %in% stables) tablename else paste(tablename, "$", sep = "")
        if(forQuery) paste("[", dbname, "]", sep="") else dbname
    } else if(ans) {
        ## we don't in general want to quote dotted names
        if(forQuery && !hasDot) quoteTabNames(channel, tablename) else tablename
    } else character(0L)
}

