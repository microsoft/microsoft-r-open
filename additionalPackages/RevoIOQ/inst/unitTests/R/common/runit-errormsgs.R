# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

### This file has two purposes:
    
## 1) to provide a check that these errors/warnings get a sensible context.
## 2) to allow translators to see their work in action.
    
### Initially it is concentrating on primitives.
### There are error messages that cannot nowadays be invoked or
### could only be invoked by calling .Internals directly.

session <- RevoIOQ:::saveRUnitSession()
options(error=expression())
   
# warning check function
"checkWarning" <- function(x)
{
  ow <- options("warn")
  options(warn=2) # turn all warnings into errors
  on.exit(options(ow))
  checkException(eval(parse(text=x)))
}

"test.arithmetic" <- function()
{    
    ## arithmetic.c
    checkException(`+`(1,2,3))
    checkException(pi + "foo")
    checkException(matrix(1:6,2,3) + matrix(1:6,3,2))
    checkException(!"foo")
    checkException(`/`(1L))
    checkException(`/`(pi))
    checkException(`/`(pi+1i))
    checkException(sin("foo"))
    checkException(trunc(pi+1i))
    checkException(atan(2,3))
    checkException(round(pi, integer(0)))
    if (compareVersion(paste(R.version$major, R.version$minor, sep=".") , "3.2.0" ) < 0) {
		checkException(log(pi, integer(0)))
	}
    checkException(log(pi, 10, 1))
    checkWarning('1:3+1:4')
    checkWarning('1e9L*1e9L')
}

"test.array" <- function()
{
    ## array.c
    checkException(matrix(1:6,2,3, byrow="foo"))
    checkException(matrix(1:6,NA,3))
    checkException(matrix(1:6,2,NA))
    checkException(matrix(1:6,-1,3))
    checkException(matrix(1:6,2,-1))
    ## the next exception occurs only on systems incapable of 
    ## allocating 64GB arrays in memory
    if ("RevoUtils" %in% .packages(all.available=TRUE))
    {
        library(RevoUtils)
        mem <- totalSystemMemory()
    }
    else
    {
        mem <- NA
    }
    ##NOTE: the following test ran without exception 
    ##  on a machine with only 32GB RAM but 64 GB Swp.
    #if (!is.na(mem) && mem < 64000000)
    if (!is.na(mem) && mem < 16000000)
    {
        checkException(matrix(NA_real_, 2^17, 2^16))
    }
    checkException(row(1))
    checkException("foo" %*% pi)
    checkException(aperm(pi))
    checkException(aperm(matrix(1:6,3.2), 3:1))
    checkException(aperm(matrix(1:6,3.2), 3:2))
    checkException(aperm(matrix(1:6,3.2), rep(1,1)))
    checkException(colSums(as.matrix(letters)))
    checkException(colSums(matrix(1:6,3.2), na.rm = NA))
}

"test.attrib" <- function()
{
    ## attrib.c
    checkException(attr(NULL, "foo") <- pi)
    checkException(attr(pi, "tsp") <- 1)
    (x <- numeric(0))
    checkException(attr(x, "tsp") <- 1:3)
    checkException(comment(x) <- pi)
    checkException(oldClass(pi) <- "factor")
    checkException(dimnames(pi) <- 1:3)
    (A <- matrix(1:6, 2, 3))
    checkException(dimnames(A) <- list(letters))
    checkException(dimnames(A) <- list(letters, NULL))
    checkException(dim(A) <- pi)
    checkException(dim(A) <- character(0))
    checkException(dim(A) <- y ~ x)
    checkException(attr(A, 1, 2, 3))
    checkException(attr(A, pi))
    checkException(attr(A, letters))
    checkException(attr(A, pi) <- pi)
    checkException(attr(A, NA_character_) <- pi)
}

"test.bind" <- function()
{
    ## bind.c
	if (compareVersion(paste(R.version$major, R.version$minor, sep="."), "3.0.2") < 0 ){
		checkException(unlist(y ~ x))
	}
    checkException(c(pi, recursive=TRUE, recursive=FALSE))
    checkException(c(list(), use.names=FALSE, use.names=TRUE))
    checkException(cbind(expression(pi), pi))
    checkWarning('cbind(1:3, 1:4)')
    checkWarning('rbind(1:3, 1:4)')  
    checkException(cbind(matrix(1:6,2,3), matrix(1:6,3,2)))
    checkException(rbind(matrix(1:6,2,3), matrix(1:6,3,2)))
}

"test.builtin" <- function()
{
    ## builtin.c
    (cat(letters, fill = -3))
    checkException(cat(letters, sep=pi))
    checkException(cat(letters, fill=3, labels=1:10))
    checkException(cat(letters, append=NA))
    checkException(cat(y ~ x))
    checkException(vector(character(0), 0))
    checkException(vector("language", 0))
    (a <- y ~ x)
    checkException(length(a) <- 5)
    (x <- pi)
    checkException(length(x) <- 1:3)
    checkException(length(x) <- NA)
    checkException(switch(1:3))
    checkException(delayedAssign(pi, "foo"))
    checkException(on.exit(ls(), add=NA_real_))
    checkException(on.exit(ls(), add=NA))
    checkException(on.exit(1,2,3))
    (x <- new.env())
    (parent.env(x) <- emptyenv())
    checkException(parent.env(x) <- pi)
    checkException(parent.env(pi) <- pi)
}

"test.character" <- function()
{
    ## character.c
    checkException(nchar(letters, type=""))
    checkException(nchar(letters, type=pi))
    checkException(substr("foo", integer(0), 1))
    (x <- pi)
    checkException(substr(x, integer(0), 1) <- pi)
    (x <- "foo")
    checkException(substr(x, integer(0), 1) <- pi)
    checkException(substr(x, 1, 1) <- pi)
    checkException(unlist(strsplit("a.b.c", "[.", perl = TRUE)))
    checkException(make.names("pi", allow_ = NA))
    checkException(grep(character(0), letters))
    checkException(grep("[.", letters))
    checkException(grep("[.", letters, perl = TRUE))
    if (compareVersion(paste(R.version$major, R.version$minor, sep=".") , "2.10.1" ) < 0) {
        checkException(sub("ab", "\\1", "abc"))
    }
    checkException(sub("", "aa", "abc", fixed=TRUE))
    (x <- "MiXeD cAsE 123")
    checkException(chartr("c-aX", "D-Fw", x))
    checkException(chartr(NA_character_, "D-Fw", x))
    checkException(chartr("ab", "c", x))
    checkException(charToRaw(pi))
    checkWarning('charToRaw(letters)')
    
    checkException(rawToChar(pi))
    checkException(rawToChar(as.raw(10), multiple=NA))
    checkException(rawShift(pi, 1))
    checkException(rawShift(as.raw(10), -20))
    checkException(rawToBits(pi))
    if (compareVersion(paste(R.version$major, R.version$minor, sep=".") , "2.8.0") < 0) {
        checkException(intToBits(pi))
    }
    checkException(strtrim(paste(letters, collapse="+"), width = -10))
}

"test.coerce" <- function()
{
    ## coerce.c
    checkException(as.vector(pi, pi))
    checkException(as.function(pi))
    checkException(as.function(list(), NULL))
    checkException(as.function(list(), pi))
    checkException(as.function(list(a=1, ls)))
    checkException(as.call(NULL))
    checkException(as.call(expression()))
    
    checkWarning('is.na(y ~ x)')
    checkWarning('is.nan(y ~ x)')
    
    checkException(call(ls))
    checkException(do.call("ls", pi))
    checkException(do.call(y~x, list()))
    checkException(do.call("ls", list(), envir=pi))
    checkException(substitute(2+4, pi))
    (x <- pi)
    checkException(storage.mode(x) <- pi)
    checkException(storage.mode(x) <- "real")
    checkException(storage.mode(x) <- "single")
    checkException(storage.mode(factor(letters)) <- "double")
    checkWarning('as.raw(1777)')
    checkException(as.integer(baseenv()))
    checkWarning('as.integer(pi+1i)')
}

"test.complex" <- function()
{
    ## complex.c
    checkException(gamma(1+1i))
    checkException(complex(-1))
    if (compareVersion(paste(R.version$major, R.version$minor, sep="."), "2.8.0") < 0 ){
        checkException(polyroot(1:50))
    }
    checkException(polyroot(c(1,2,NA)))
}

"test.cum" <- function()
{
    ## cum.c
    checkException(cummin(1+1i))
    checkException(cummax(1+1i))
}

"test.debug" <- function()
{
    ## debug.c
    if (compareVersion(paste(R.version$major, R.version$minor, sep=".") , "2.10.1" ) < 0) {
        checkException(debug(is.na))
    }
    checkWarning('undebug(ls)')
    
    checkException(trace(y ~ x))
    checkException(tracemem(ls))
    checkException(tracemem(NULL))
    checkException(tracemem(baseenv()))
    checkException(untracemem(ls))
    
    if (!capabilities()["profmem"]) 
    {
      checkTrue(is.null(retracemem()))
      checkTrue(is.null(retracemem(ls)))
      checkTrue(is.null(retracemem(pi, 1, 2)))
      checkTrue(is.null(retracemem(pi, pi)))
    }
    else
    {
      if (compareVersion(paste(R.version$major, R.version$minor, sep=".") , "2.11.0" ) < 0 ) {
	      checkException(retracemem())
      }
	  if (compareVersion(paste(R.version$major, R.version$minor, sep=".") , "2.13.2" ) < 0 ) {
	      checkException(retracemem(ls))
	  }
      checkException(retracemem(pi, 1, 2))
	  if (compareVersion(paste(R.version$major, R.version$minor, sep=".") , "2.13.2" ) < 0 ) {
		  checkException(retracemem(pi, pi))
	  }
    }
}

"test.envir" <- function()
{
    ## envir.c
    checkException(as.environment(NULL))
    checkException(as.environment(y ~ x))
    checkException(as.environment("foo"))
    checkException(assign(pi, pi))
    (assign("pi", pi))
    checkException(assign("pi", pi, envir=list()))
    checkException(assign("pi", pi, inherits=NA_real_))
    checkException(remove("x", envir=list()))
    checkException(remove("x", inherits=NA_real_))
    (remove("xxx"))
    checkException(get(pi))
    checkException(get(""))
    checkException(get("pi", envir=list()))
    checkException(get("pi", inherits=NA_real_))
    checkException(get("pi", mode=pi))
    checkException(get("pi", mode="foo"))
    checkException(get("xxx", mode="any"))
    checkException(mget(pi))
    checkException(mget(letters, envir=list()))
    checkException(mget(letters, baseenv(), inherits=NA))
    checkException(mget("pi", baseenv(), mode=pi))
    checkException(mget("pi", baseenv(), mode="foo"))
    checkException(missing(3))
    checkException(attach(list(), pos="foo"))
    checkException(attach(list(), name=pi))
    checkException(attach(list(pi)))
    checkException(attach(pi))
    checkException(detach("package:base"))
    checkException(detach(pi))
    checkException(ls(envir = y ~ x))
    checkException(pos.to.env(integer(0)))
    checkException(pos.to.env(0))
    checkException(as.list.environment(pi))
}

"test.eval" <- function()
{
    ## eval.c
    (if(rep(TRUE, 10)) "foo")
    (f <- function() return(1,2,3))
    if (compareVersion(paste(R.version$major, R.version$minor, sep=".") , "2.11.0" ) < 0) {
        (x <- f())
    } else {
            checkException(x <- f())
    }
    (f <- function() return(1,,3))
    checkException(x <- f())
}

"test.main" <- function()
{
    ## main.c
	if (commandArgs()[1]=="" && interactive()) # if we are running the IDE
	{
		# q() behaves differently under the IDE
		return(TRUE)
	} else {
		checkException(q(pi))
		checkException(q("foo"))
	}
}

"test.names" <- function()
{
    ## names.c
    checkException(.Primitive(pi))
    checkException(.Primitive("foo"))
    checkException(.Internal(pi))
    checkException(.Internal(pairlist(list)))
}

"test.objects" <- function()
{
    ## objects.c
    checkException(UseMethod())
    f <- function(x) UseMethod(); checkException(f(pi))
    f <- function(x) UseMethod(ls); checkException(f(pi))
    f <- function(x) UseMethod("coef"); (f(list(coefficients=pi)))
    f <- function(x) UseMethod("coef", x); (f(list(coefficients=pi)))
    checkWarning('f <- function(x) UseMethod("coef", x, pi); (f(list(coefficients=pi)))')
    
    f <- function(x) UseMethod("cc"); checkException(f(list(coefficients=pi)))
	if (compareVersion(paste(R.version$major, R.version$minor, sep=".") , "3.2.0" ) < 0) {	
		checkException(unclass(baseenv()))
	}
    checkException(inherits(pi, pi))
    checkException(inherits(pi, "factor", pi))
    checkException(standardGeneric(pi))
}

"test.random" <- function()
{
    ## random.c
    checkException(runif(-1, 0, 1))
    checkException(sample(10, replace=logical(0)))
    checkException(sample(10, replace=NA))
    checkException(sample(1:10, -1))
    checkException(sample(1:10, 20, replace=FALSE))
    checkException(sample(1:10, 3, prob=rep(0.2,5)))
    checkException(rmultinom(-1, 1, rep(0.2, 5)))
    checkException(rmultinom(1, -1, rep(0.2, 5)))
}

"test.seq" <- function()
{
    ## seq.c
    checkException(factor(1:3) : factor(1:4))
    checkException(1:1e20)
    (x <- 2:3)
    checkWarning('x:1')
    checkWarning('1:x')
    checkException(1:NA)
    checkException(rep.int(pi, -1))
    checkException(rep.int(c(pi,pi), c(-1,-2)))
    checkException(rep.int(y ~ x, 2))
    checkException(rep.int(2, y ~ x))
    checkException(rep.int(1:3, 1:2))
    checkException(rep(pi, length.out = -1))
    checkException(rep(pi, each = -1))
    checkException(rep(pi, times=NA))
    checkException(seq.int(1, length.out=-3))
    checkException(seq.int(1, length.out=NA))
    checkException(seq.int(Inf, 1, 2))
    checkException(seq.int(1, Inf, 2))
    checkException(seq.int(1, 2, NA))
    checkException(seq.int(1.2, 1, by=1))
    checkException(seq.int(1, 2, 3, 4, 5))
    checkException(seq_len(-1))
}

"test.arity" <- function()
{
    ## util.c
    ## arity checks
    checkException(sin(1,2))
    checkException(.Internal(unique(pi)))
    checkException(setwd(pi))
    checkException(setwd("/non-existent"))
    checkException(basename(pi))
    checkException(dirname(pi))
    checkException(encodeString(pi, -1))
    checkException(encodeString(pi, 10, quote=pi))
    (encodeString(pi, 10, quote="abc"))
    checkException(encodeString(pi, 10, na.encode=NA))
    checkException(Encoding(pi))
    checkException(Encoding(pi) <- pi)
    (x <- "foo")
    checkException(Encoding(x) <- pi)
    checkException(Encoding(x) <- character(0))
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

