#
# Copyright (c) 2008-2010 Revolution Analytics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# This function makes iterator makers.  The resulting iterator makers all take
# an optional "count" argument which specifies the number of times the
# resulting iterator should fire.  The iterators are wrappers around functions
# that return different values each time they are called.  All this is done to
# avoid cutting and pasting the same code repeatedly.  We could make this
# function available to the user, but I'm not sure if we will immediately.
makeIwrapper <- function(FUN) {
  function(..., count) {
    if (!missing(count) && (!is.numeric(count) || length(count) != 1))
      stop('count must be a numeric value')

    # construct the call object to put into the nextElem function
    m <- as.call(c(as.name(FUN), list(...)))

    # construct the body of the nextElem function
    fbody <- if (missing(count)) {
      m
    } else {
      substitute({
        if (count > 0) {
          count <<- count - 1L
          REPLACETHIS
        } else {
          stop('StopIteration', call.=FALSE)
        }
      }, list(REPLACETHIS=m))
    }

    # create the nextElem function using fbody
    nextEl <- function() NULL
    body(nextEl) <- fbody

    # create and return the iterator object
    it <- list(nextElem=nextEl)
    class(it) <- c('abstractiter', 'iter')
    it
  }
}

# define some iterator makers using makeIwrapper
irunif <- makeIwrapper('runif')
irnorm <- makeIwrapper('rnorm')
irbinom <- makeIwrapper('rbinom')
irnbinom <- makeIwrapper('rnbinom')
irpois <- makeIwrapper('rpois')
isample <- makeIwrapper('sample')  # not in the NAMESPACE currently

# a counting iterator
icount <- function(count) {
  if (missing(count))
    count <- NULL
  else if (!is.numeric(count) || length(count) != 1)
    stop('count must be a numeric value')

  i <- 0L

  nextEl <- function() {
    if (is.null(count) || i < count)
      (i <<- i + 1L)
    else
      stop('StopIteration', call.=FALSE)
  }

  it <- list(nextElem=nextEl)
  class(it) <- c('abstractiter', 'iter')
  it
}

# an iterator over pieces of a number
idiv <- function(n, ..., chunks, chunkSize) {
  if (!is.numeric(n) || length(n) != 1)
    stop('n must be a numeric value')

  if (length(list(...)) > 0)
    stop('chunks and chunkSize must be specified as named arguments')

  if ((missing(chunkSize) && missing(chunks)) ||
      (!missing(chunkSize) && !missing(chunks)))
    stop('either chunks or chunkSize must be specified, but not both')

  if (missing(chunks)) {
    if (!is.numeric(chunkSize) || length(chunkSize) != 1 || chunkSize < 1)
      stop('chunkSize must be a numeric value >= 1')
    chunks <- ceiling(n / chunkSize)
  }

  nextEl <- function() {
    if (chunks <= 0 || n <= 0)
      stop('StopIteration', call.=FALSE)

    m <- ceiling(n / chunks)
    n <<- n - m
    chunks <<- chunks - 1
    m
  }

  it <- list(nextElem=nextEl)
  class(it) <- c('abstractiter', 'iter')
  it
}

# an iterator over text lines from a connection
ireadLines <- function(con, n=1, ...) {
  if (!is.numeric(n) || length(n) != 1 || n < 1)
    stop('n must be a numeric value >= 1')

  if (is.character(con)) {
    con <- file(con, open='r')
    doClose <- TRUE
  } else {
    doClose <- FALSE
  }

  nextEl <- function() {
    if (is.null(con))
      stop('StopIteration', call.=FALSE)

    r <- readLines(con, n=n, ...)
    if (length(r) == 0) {
      if (doClose)
        close(con)
      con <<- NULL
      stop('StopIteration', call.=FALSE)
    }
    r
  }

  it <- list(nextElem=nextEl)
  class(it) <- c('abstractiter', 'iter')
  it
}

# an iterator over rows of a data frame read from a file
iread.table <- function(file, ..., verbose=FALSE) {
  args <- list(...)
  argnames <- names(args)

  # need to do this (at least for now) because the default values for
  # header and row.names depend on the first few lines of the file,
  # which could cause a different number of columns to be returned from
  # the first versus the subsequent calls to read.table
  if (!all(c('header', 'row.names') %in% argnames))
    stop('both header and row.names must be specified in this implementation')

  nrows <- if ('nrows' %in% argnames) args$nrows else 1
  row.names <- args$row.names

  # it doesn't seem to make sense to allow nrows < 1 for the "iterator"
  # version of read.table
  if (!is.numeric(nrows) || length(nrows) != 1 || nrows < 1)
    stop('nrows must be a numeric value >= 1')

  # open the file if necessary and remember to close it
  if (is.character(file)) {
    file <- file(file, open='r')
    doClose <- TRUE
  } else {
    doClose <- FALSE
  }

  # create the call object that we'll use to call read.table
  m <- as.call(c(as.name('read.table'), file='', list(...)))
  m$file <- file
  m$nrows <- nrows  # needed since we use a different default than read.table
  env <- sys.frame(sys.nframe())

  # compute these once rather than repeatedly
  rnlen <- length(row.names)
  gotrownames <- is.character(row.names) && rnlen > 1

  # initialize a few state variables
  first.time <- TRUE
  irow <- 1
  errmsg <- NULL

  nextEl <- function() {
    if (!is.null(errmsg))
      stop(paste('iterator failed previously:', errmsg), call.=FALSE)

    if (is.null(file))
      stop('StopIteration', call.=FALSE)

    if (gotrownames) {
      rem <- rnlen - irow + 1  # remaining strings in row.names
      nrows <<- min(nrows, rem)  # possibly decrease nrows to match row.names

      # there is a problem if nrows is one: we would have to set row.names
      # to a character vector of length one, which is interpreted
      # incorrectly by read.table
      if (nrows > 1)
        m$row.names <<- row.names[seq(irow, length=nrows)]
      else
        m['row.names'] <<- list(NULL)  # we'll fix the row names later
      m$nrows <<- nrows
    }

    # call read.table to actually read the file
    r <- tryCatch({
      # handle the case where we've run out of row names
      if (nrows > 0) {
        if (verbose)
          print(m)
        eval(m, env)
      } else {
        NULL
      }
    },
    error=function(e) {
      # this error is thrown at the end of input sometimes
      # but other times a data frame with no rows is returned
      # (for instance when col.names is specified)
      if (!identical(conditionMessage(e), 'no lines available in input')) {
        if (doClose)
          close(file)
        file <<- NULL
        errmsg <<- conditionMessage(e)
        stop(e)
      }
      NULL
    })

    # set header to FALSE, skip to 0, and col.names to names(r)
    # after the first call to read.table
    if (first.time) {
      first.time <<- FALSE
      m$header <<- FALSE
      m$skip <<- 0
      nms <- names(r)
      if (is.numeric(row.names)) {
        nms <- if (row.names == 1)
          c('', nms)
        else if (row.names >= length(nms))
          c(nms, '')
        else
          c(nms[1:(row.names-1)], '', nms[row.names:length(nms)])
      }
      m$col.names <<- nms
    }

    # check if we're done reading
    if (is.null(r) || nrow(r) == 0) {
      if (doClose)
        close(file)
      file <<- NULL
      stop('StopIteration', call.=FALSE)
    }

    if (gotrownames) {
      # fix the row names for this particular case
      if (nrows == 1)
        rownames(r) <- row.names[irow]

      # update the index into row.names
      irow <<- irow + nrows
    }

    r
  }

  it <- list(nextElem=nextEl)
  class(it) <- c('abstractiter', 'iter')
  it
}
