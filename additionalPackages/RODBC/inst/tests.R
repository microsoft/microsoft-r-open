# These tests are only for BDR's Windows & Linux systems

library(RODBC)
library(MASS)
USArrests[1,2] <- NA
hills <- hills[1:15,]
row.names(hills)[12] <- "Dollar ('$')"
set.seed(1)

# MySQL
## testdb3 is ODBC/Connector 3.51.x, testdb5 is 5.1.x
channel <- odbcConnect("testdb3")
odbcGetInfo(channel)
sqlTypeInfo(channel)
sqlTables(channel)
sqlDrop(channel, "USArrests", errors = FALSE)
sqlSave(channel, USArrests, rownames = "state", addPK = TRUE)
sqlTables(channel)
sqlColumns(channel, "USArrests")
sqlColumns(channel, "USArrests", special = TRUE)
sqlPrimaryKeys(channel, "USArrests")
sqlFetch(channel, "USArrests", rownames = "state")
sqlQuery(channel, "select state, murder from USArrests where rape > 30 order by murder")
foo <- cbind(state=row.names(USArrests), USArrests)[1:3, c(1,3)]
foo[1,2] <- 236
sqlUpdate(channel, foo, "USArrests")
sqlFetch(channel, "USArrests", rownames = "state", max = 5)
sqlFetchMore(channel, rownames = "state", max = 8)
sqlDrop(channel, "USArrests")

Btest <- Atest <-
    data.frame(x = c(paste(1:100, collapse="+"), letters[2:4]), rn=1:4)
Btest[,1] <- Atest[c(4,1:3),1]
sqlDrop(channel, "Atest", errors = FALSE)
colspec <- list(character="mediumtext", double="double",
                integer="integer", logical="varchar(5)")
sqlSave(channel, Atest, typeInfo = colspec)
sqlColumns(channel, "Atest")
sqlFetch(channel, "Atest")
sqlUpdate(channel, Btest, "Atest", index = "rn")
sqlFetch(channel, "Atest")
sqlDrop(channel, "Atest")
varspec <- "mediumtext"; names(varspec) <- "x"
sqlSave(channel, Atest, varTypes = varspec)
sqlColumns(channel, "Atest")
sqlFetch(channel, "Atest")
sqlDrop(channel, "Atest")

dates <- as.character(seq(as.Date("2004-01-01"), by="week", length=10))
times <- paste(1:10, "05", "00", sep=":")
dt <- as.POSIXct(paste(dates, times))
Dtest <- data.frame(dates, times, dt, logi=c(TRUE, NA, FALSE, FALSE, FALSE))
varspec <- c("date", "time", "timestamp", "varchar(5)")
names(varspec) <- names(Dtest)
sqlSave(channel, Dtest, varTypes = varspec, verbose=TRUE)
sqlColumns(channel, "Dtest")
sqlFetch(channel, "Dtest")
sqlDrop(channel, "Dtest")

sqlDrop(channel, "hills test", errors = FALSE)
sqlSave(channel, hills, "hills test", verbose=TRUE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=TRUE)
sqlFetch(channel, "hills test")
sqlDrop(channel, "hills test")
sqlSave(channel, hills, "hills test", verbose=TRUE, fast=FALSE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=FALSE)
sqlDrop(channel, "hills test")
close(channel)


# Access
channel <- odbcConnect("testacc")
odbcGetInfo(channel)
sqlTypeInfo(channel)
sqlTables(channel)
sqlDrop(channel, "USArrests", errors = FALSE)
sqlSave(channel, USArrests)
sqlTables(channel)
sqlColumns(channel, "USArrests")
sqlFetch(channel, "USArrests")
query <- paste("select rownames, murder from USArrests",
               "where Rape > 30",  "order by Murder")
sqlQuery(channel, query)
sqlCopy(channel, query, "HighRape", rownames = FALSE)
sqlFetch(channel, "HighRape", max = 5)
sqlTables(channel)
sqlDrop(channel, "HighRape")
foo <-  USArrests[1:3, 2, drop = FALSE]
foo[1,1] <- 236
sqlUpdate(channel, foo, "USArrests")
sqlFetch(channel, "USArrests", max = 5)
sqlFetchMore(channel, max = 8)
sqlDrop(channel, "USArrests")

dates <- as.character(seq(as.Date("2004-01-01"), by="week", length=10))
Dtest <- data.frame(dates)
sqlDrop(channel, "Dtest", errors = FALSE)
varspec <- "DATETIME"; names(varspec) <- names(Dtest)
sqlSave(channel, Dtest, varTypes = varspec, verbose=TRUE, fast=FALSE)
# fast = TRUE crashes
sqlColumns(channel, "Dtest")
sqlFetch(channel, "Dtest")
sqlDrop(channel, "Dtest")

sqlDrop(channel, "hills test", errors = FALSE)
sqlSave(channel, hills, "hills test", verbose=TRUE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=TRUE)
sqlFetch(channel, "hills test")
sqlDrop(channel, "hills test")
sqlSave(channel, hills, "hills test", verbose=TRUE, fast=FALSE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=FALSE)
sqlDrop(channel, "hills test")
close(channel)


# Excel
channel <- odbcConnectExcel("hills.xls")
## list the spreadsheets and marked ranges
sqlTables(channel)
sqlColumns(channel, "hills")
## two ways to retrieve the contents of hills
sqlFetch(channel, "hills")
hills2 <- sqlQuery(channel, "select * from [hills$]")

sqlFetch(channel, "testit")
close(channel)

# The Excel driver maps ' ' to '_'.
channel <- odbcConnectExcel("/bdr/hills.xls", readOnly=FALSE)
sqlSave(channel, hills, "hills_test", verbose=TRUE, fast=FALSE)
sqlUpdate(channel, hills[11:15,], "hills_test", verbose=TRUE, fast=FALSE)
sqlFetch(channel, "hills_test")
sqlSave(channel, hills, "hills_test2", verbose=TRUE)
sqlUpdate(channel, hills[11:15,], "hills_test2", verbose=TRUE)
sqlFetch(channel, "hills_test2")
close(channel)


# DBase: maps table/column names to u/case, max length 8
dbf <- system.file("files", "sids.dbf", package="foreign")
channel <- odbcConnectDbase(dbf)
(sids <- sqlFetch(channel, "sids"))
sqlUpdate(channel, sids[1:2, ], "sids", index="NAME", verbose=TRUE, fast=FALSE)
close(channel)
channel <- odbcConnectDbase(dbf, case="toupper")
sqlSave(channel, hills, "HILLS 2", verbose=TRUE)
sqlUpdate(channel, hills[11:15,],  "HILLS 2", verbose=TRUE)
sqlDrop(channel, "HILLS 2")
close(channel)


# SQL Server 2008 Express Edition
channel <- odbcConnect("SQLServer")
odbcGetInfo(channel)
sqlTypeInfo(channel)
sqlTables(channel)
sqlDrop(channel, "USArrests", errors = FALSE)
sqlSave(channel, USArrests, addPK = TRUE)
sqlTables(channel)
sqlColumns(channel, "USArrests")
sqlPrimaryKeys(channel, "USArrests")
sqlFetch(channel, "USArrests")
query <- paste("select rownames, murder from USArrests",
               "where Rape > 30",  "order by Murder")
sqlQuery(channel, query)
sqlCopy(channel, query, "HighRape", rownames = FALSE)
sqlFetch(channel, "HighRape", max = 5)
sqlTables(channel)
sqlDrop(channel, "HighRape")
foo <-  USArrests[1:3, 2, drop = FALSE]
foo[1,1] <- 236
sqlUpdate(channel, foo, "USArrests")
sqlFetch(channel, "USArrests", max = 5)
sqlFetchMore(channel, max = 8)
sqlDrop(channel, "USArrests")

dates <- as.character(seq(as.Date("2004-01-01"), by="week", length=10))
times <- paste(1:10, "05", "00", sep=":")
Dtest <- data.frame(dates, times, logi=c(TRUE, NA, FALSE, FALSE, FALSE))
sqlDrop(channel, "Dtest", errors = FALSE)
varspec <- c("smalldatetime", "smalldatetime", "varchar(5)")
names(varspec) <- names(Dtest)
# fast = TRUE fails, claims data is invalid
sqlSave(channel, Dtest, varTypes = varspec, verbose=TRUE, fast=FALSE)
sqlColumns(channel, "Dtest")
sqlFetch(channel, "Dtest")
# This retrieves TRUE as " TRUE" and so returns a factor.
sqlDrop(channel, "Dtest")

sqlDrop(channel, "hills test", errors = FALSE)
sqlSave(channel, hills, "hills test", verbose=TRUE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=TRUE)
sqlFetch(channel, "hills test")
sqlDrop(channel, "hills test")
sqlSave(channel, hills, "hills test", verbose=TRUE, fast=FALSE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=FALSE)
sqlDrop(channel, "hills test")
close(channel)


# PostgreSQL 8.x on Windows
channel <- odbcConnect("testpg")
odbcGetInfo(channel)
sqlTypeInfo(channel)
sqlTables(channel)
sqlDrop(channel, "USArrests", errors = FALSE)
sqlSave(channel, USArrests, rownames = "state", addPK = TRUE)
sqlTables(channel)
sqlColumns(channel, "USArrests")
sqlColumns(channel, "USArrests", special = TRUE)
sqlPrimaryKeys(channel, "USArrests")
sqlFetch(channel, "USArrests", rownames = "state")
sqlQuery(channel, "select state, murder from USArrests where rape > 30 order by murder")
foo <- cbind(state=row.names(USArrests), USArrests)[1:3, c(1,3)]
foo[1,2] <- 236
sqlUpdate(channel, foo, "USArrests", index = "state")
sqlFetch(channel, "USArrests", rownames = "state", max = 5)
sqlFetchMore(channel, rownames = "state", max = 8)
sqlDrop(channel, "USArrests")

dates <- as.character(seq(as.Date("2004-01-01"), by="week", length=10))
times <- paste(1:10, "05", "00", sep=":")
Dtest <- data.frame(dates, times, logi=c(TRUE, NA, FALSE, FALSE, FALSE))
sqlDrop(channel, "Dtest", errors = FALSE)
varspec <- c("date", "time", "varchar(5)"); names(varspec) <- names(Dtest)
sqlSave(channel, Dtest, varTypes = varspec, verbose=TRUE)
sqlColumns(channel, "Dtest")
sqlFetch(channel, "Dtest")
sqlDrop(channel, "Dtest")

sqlDrop(channel, "hills test", errors = FALSE)
sqlSave(channel, hills, "hills test", verbose=TRUE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=TRUE)
sqlFetch(channel, "hills test")
sqlDrop(channel, "hills test")
sqlSave(channel, hills, "hills test", verbose=TRUE, fast=FALSE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=FALSE)
sqlDrop(channel, "hills test")

Btest <- Atest <-
    data.frame(x = c(paste(1:100, collapse="+"), letters[2:4]), rn=1:4)
Btest[,1] <- Atest[c(4,1:3),1]
sqlDrop(channel, "Atest", errors = FALSE)
# sqlSave(channel, Atest, addPK = TRUE)
colspec <- list(character="text", double="double",
                integer="integer", logical="varchar(5)")
sqlSave(channel, Atest, typeInfo = colspec)
sqlColumns(channel, "Atest")
sqlFetch(channel, "Atest")
sqlUpdate(channel, Btest, "Atest", index = "rn")
sqlFetch(channel, "Atest")
sqlDrop(channel, "Atest")
varspec <- "text"; names(varspec) <- "x"
sqlSave(channel, Atest, varTypes = varspec)
sqlColumns(channel, "Atest")
sqlFetch(channel, "Atest")
sqlDrop(channel, "Atest")
close(channel)


# SQLite 3.x.y
channel <- odbcConnect("sqlite3")
odbcGetInfo(channel)
sqlTypeInfo(channel)
sqlTables(channel)
sqlDrop(channel, "USArrests", errors = FALSE)
sqlSave(channel, USArrests)
sqlTables(channel)
sqlColumns(channel, "USArrests")
sqlFetch(channel, "USArrests")
sqlQuery(channel, "select rownames, Murder from USArrests where Rape > 30 order by Murder")
foo <-  USArrests[1:3, 2, drop = FALSE]
foo[1,1] <- 236
sqlUpdate(channel, foo, "USArrests")
sqlFetch(channel, "USArrests", max = 5)
sqlFetchMore(channel, max = 8)
sqlDrop(channel, "USArrests")

sqlDrop(channel, "hills test", errors = FALSE)
sqlSave(channel, hills, "hills test", verbose=TRUE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=TRUE)
sqlFetch(channel, "hills test")
sqlDrop(channel, "hills test")
sqlSave(channel, hills, "hills test", verbose=TRUE, fast=FALSE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=FALSE)
sqlDrop(channel, "hills test")

dates <- as.character(seq(as.Date("2004-01-01"), by="week", length=10))
times <- paste(1:10, "05", "00", sep=":")
dt <- paste(dates, times)
Dtest <- data.frame(dates, times, dt, logi=c(TRUE, NA, FALSE, FALSE, FALSE))
sqlDrop(channel, "Dtest", errors = FALSE)
varspec <- c("date", "time", "timestamp", "varchar(5)")
names(varspec) <- names(Dtest)
sqlSave(channel, Dtest, varTypes = varspec, fast=FALSE)
# fast=TRUE fails
sqlColumns(channel, "Dtest")
sqlFetch(channel, "Dtest")
# This retrieves TRUE as " TRUE" and so returns a factor.
sqlDrop(channel, "Dtest")
close(channel)




###---------------------------------------------------------------------

# MySQL on Unix
channel <- odbcConnect("test")
odbcGetInfo(channel)
sqlTypeInfo(channel)
sqlTables(channel)
sqlDrop(channel, "USArrests", errors = FALSE)
sqlSave(channel, USArrests, rownames = "State", addPK = TRUE)
sqlTables(channel)
sqlColumns(channel, "USArrests")
sqlColumns(channel, "USArrests", special = TRUE)
sqlPrimaryKeys(channel, "USArrests")
sqlFetch(channel, "USArrests", rownames = "State")
sqlQuery(channel, "select State, Murder from USArrests where Rape > 30 order by Murder")
foo <- cbind(State=row.names(USArrests), USArrests)[1:3, c(1,3)]
foo[1,2] <- 236
sqlUpdate(channel, foo, "USArrests")
sqlFetch(channel, "USArrests", rownames = "State", max = 5)
sqlDrop(channel, "USArrests")

Btest <- Atest <-
    data.frame(x = c(paste(1:100, collapse="+"), letters[2:4]), rn=1:4)
Btest[,1] <- Atest[c(4,1:3),1]
sqlDrop(channel, "Atest", errors = FALSE)
# sqlSave(channel, Atest, addPK = TRUE)
colspec <- list(character="mediumtext", double="double",
                integer="integer", logical="varchar(5)")
sqlSave(channel, Atest, typeInfo = colspec)
sqlColumns(channel, "Atest")
sqlFetch(channel, "Atest")
sqlUpdate(channel, Btest, "Atest", index = "rn")
sqlFetch(channel, "Atest")
sqlDrop(channel, "Atest")
varspec <- "mediumtext"; names(varspec) <- "x"
sqlSave(channel, Atest, varTypes = varspec)
sqlColumns(channel, "Atest")
sqlFetch(channel, "Atest")
sqlDrop(channel, "Atest")

dates <- as.character(seq(as.Date("2004-01-01"), by="week", length=10))
times <- paste(1:10, "05", "00", sep=":")
dt <- as.POSIXct(paste(dates, times))
Dtest <- data.frame(dates, times, dt, logi=c(TRUE, NA, FALSE, FALSE, FALSE))
sqlDrop(channel, "Dtest", errors = FALSE)
varspec <- c("date", "time", "timestamp", "varchar(5)")
names(varspec) <- names(Dtest)
sqlDrop(channel, "Dtest", errors = FALSE)
sqlSave(channel, Dtest, varTypes = varspec, verbose=TRUE)
sqlColumns(channel, "Dtest")
sqlFetch(channel, "Dtest")
sqlDrop(channel, "Dtest")

sqlDrop(channel, "hills test", errors = FALSE)
sqlSave(channel, hills, "hills test", verbose=TRUE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=TRUE)
sqlFetch(channel, "hills test")
sqlDrop(channel, "hills test")
sqlSave(channel, hills, "hills test", verbose=TRUE, fast=FALSE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=FALSE)
sqlDrop(channel, "hills test")
close(channel)


# sqlite on Unix
channel <- odbcConnect("sqlite3")
odbcGetInfo(channel)
sqlTypeInfo(channel)
sqlTables(channel)
sqlDrop(channel, "USArrests", errors = FALSE)
sqlSave(channel, USArrests)
sqlTables(channel)
sqlColumns(channel, "USArrests")
sqlFetch(channel, "USArrests")
sqlQuery(channel, "select rownames, Murder from USArrests where Rape > 30 order by Murder")
foo <-  USArrests[1:3, 2, drop = FALSE]
foo[1,1] <- 236
sqlUpdate(channel, foo, "USArrests")
sqlFetch(channel, "USArrests", max = 5)
sqlDrop(channel, "USArrests")

sqlDrop(channel, "hills test", errors = FALSE)
sqlSave(channel, hills, "hills test", verbose=TRUE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=TRUE)
sqlFetch(channel, "hills test")
sqlDrop(channel, "hills test")
sqlSave(channel, hills, "hills test", verbose=TRUE, fast=FALSE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=FALSE)
sqlDrop(channel, "hills test")

A <- data.frame(a="«Latin-1 accented chars»: éè øØ å<Å æ<Æ")
sqlDrop(channel, "A", errors = FALSE)
sqlSave(channel, A, verbose=TRUE)
sqlFetch(channel, "A")
## close the connection
close(channel)


# PostgreSQL on Unix
channel <- odbcConnect("testpg")
odbcGetInfo(channel)
sqlTypeInfo(channel)
sqlTables(channel)
sqlDrop(channel, "USArrests", errors = FALSE)
sqlSave(channel, USArrests, rownames = "state", addPK = TRUE)
sqlTables(channel)
sqlColumns(channel, "USArrests")
sqlColumns(channel, "USArrests", special = TRUE)
sqlPrimaryKeys(channel, "USArrests")
sqlFetch(channel, "USArrests", rownames = "state")
sqlQuery(channel, "select state, murder from USArrests where rape > 30 order by murder")
foo <- cbind(state=row.names(USArrests), USArrests)[1:3, c(1,3)]
foo[1,2] <- 236
sqlUpdate(channel, foo, "USArrests", index = "state")
sqlFetch(channel, "USArrests", rownames = "state", max = 5)
sqlDrop(channel, "USArrests")

sqlDrop(channel, "hills test", errors = FALSE)
sqlSave(channel, hills, "hills test", verbose=TRUE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=TRUE)
sqlFetch(channel, "hills test")
sqlDrop(channel, "hills test")
sqlSave(channel, hills, "hills test", verbose=TRUE, fast=FALSE)
sqlUpdate(channel, hills[11:15,], "hills test", verbose=TRUE, fast=FALSE)
sqlDrop(channel, "hills test")
## close the connection
close(channel)

channel <- odbcConnect("testpg")
dates <- as.character(seq(as.Date("2004-01-01"), by="week", length=10))
times <- paste(1:10, "05", "00", sep=":")
dt <- paste(dates, " ", times, ".", round(1000*runif(10)), sep="")
Dtest <- data.frame(dates, times, dt, logi=c(TRUE, NA, FALSE, FALSE, FALSE))
sqlDrop(channel, "Dtest", errors = FALSE)
varspec <- c("date", "time", "timestamp", "varchar(5)")
names(varspec) <- names(Dtest)
sqlSave(channel, Dtest, varTypes = varspec, verbose=TRUE)
sqlColumns(channel, "Dtest")
sqlFetch(channel, "Dtest")
sqlDrop(channel, "Dtest")
close(channel)

channel <- odbcConnect("testpg")
Btest <- Atest <-
    data.frame(x = c(paste(1:100, collapse="+"), letters[2:4]), rn=1:4)
Btest[,1] <- Atest[c(4,1:3),1]
sqlDrop(channel, "Atest", errors = FALSE)
colspec <- list(character="text", double="double",
                integer="integer", logical="varchar(5)")
sqlSave(channel, Atest, typeInfo = colspec)
sqlColumns(channel, "Atest")
sqlFetch(channel, "Atest")
sqlUpdate(channel, Btest, "Atest", index = "rn")
sqlFetch(channel, "Atest")
sqlDrop(channel, "Atest")
varspec <- "text"; names(varspec) <- "x"
sqlSave(channel, Atest, varTypes = varspec)
sqlColumns(channel, "Atest")
sqlFetch(channel, "Atest")
sqlDrop(channel, "Atest")
close(channel)

