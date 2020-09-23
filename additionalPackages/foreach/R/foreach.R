#
# Copyright (c) Microsoft. All rights reserved.
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

accumulate <- function(obj, result, tag, ...) {
  UseMethod('accumulate')
}

getResult <- function(obj, ...) {
  UseMethod('getResult')
}

getErrorValue <- function(obj, ...) {
  UseMethod('getErrorValue')
}

getErrorIndex <- function(obj, ...) {
  UseMethod('getErrorIndex')
}

defcombine <- function(a, ...) c(a, list(...))

foreach <- function(..., .combine, .init, .final=NULL, .inorder=TRUE,
                    .multicombine=FALSE,
                    .maxcombine=if (.multicombine) 100 else 2,
                    .errorhandling=c('stop', 'remove', 'pass'),
                    .packages=NULL, .export=NULL, .noexport=NULL,
                    .verbose=FALSE) {
  if (missing(.combine)) {
    if (!missing(.init))
      stop('if .init is specified, then .combine must also be specified')
    .combine <- defcombine
    hasInit <- TRUE
    init <- quote(list())
  } else {
    .combine <- match.fun(.combine)
    if (missing(.init)) {
      hasInit <- FALSE
      init <- NULL
    } else {
      hasInit <- TRUE
      init <- substitute(.init)
    }
  }

  # .multicombine defaults to TRUE if the .combine function is known to
  # take multiple arguments
  if (missing(.multicombine) &&
      (identical(.combine, cbind) ||
       identical(.combine, rbind) ||
       identical(.combine, c) ||
       identical(.combine, defcombine)))
    .multicombine <- TRUE

  # sanity check the arguments
  if (!is.null(.final) && !is.function(.final))
    stop('.final must be a function')
  if (!is.logical(.inorder) || length(.inorder) > 1)
    stop('.inorder must be a logical value')
  if (!is.logical(.multicombine) || length(.multicombine) > 1)
    stop('.multicombine must be a logical value')
  if (!is.numeric(.maxcombine) || length(.maxcombine) > 1 || .maxcombine < 2)
    stop('.maxcombine must be a numeric value >= 2')
  if (!is.character(.errorhandling))
    stop('.errorhandling must be a character string')
  if (!is.null(.packages) && !is.character(.packages))
    stop('.packages must be a character vector')
  if (!is.null(.export) && !is.character(.export))
    stop('.export must be a character vector')
  if (!is.null(.noexport) && !is.character(.noexport))
    stop('.noexport must be a character vector')
  if (!is.logical(.verbose) || length(.verbose) > 1)
    stop('.verbose must be a logical value')

  specified <- c('errorHandling', 'verbose')
  specified <- specified[c(!missing(.errorhandling), !missing(.verbose))]

  args <- substitute(list(...))[-1]

  if (length(args) == 0)
    stop('no iteration arguments specified')
  argnames <- names(args)
  if (is.null(argnames))
    argnames <- rep('', length(args))

  # check for backend-specific options
  options <- list()
  opts <- grep('^\\.options\\.[A-Za-z][A-Za-z]*$', argnames)
  if (length(opts) > 0) {
    # put the specified options objects into the options list
    for (i in opts) {
      bname <- substr(argnames[i], 10, 100)
      options[[bname]] <- list(...)[[i]]
    }

    # remove the specified options objects from args and argnames
    args <- args[-opts]
    argnames <- argnames[-opts]
  }

  # check for arguments that start with a '.', and issue an error,
  # assuming that these are misspelled options
  unrecog <- grep('^\\.', argnames)
  if (length(unrecog) > 0)
    stop(sprintf('unrecognized argument(s): %s',
                 paste(argnames[unrecog], collapse=', ')))

  # check for use of old-style arguments, and issue an error
  oldargs <- c('COMBINE', 'INIT', 'INORDER', 'MULTICOMBINE', 'MAXCOMBINE',
               'ERRORHANDLING', 'PACKAGES', 'VERBOSE', 'EXPORT', 'NOEXPORT',
               'LOADFACTOR', 'CHUNKSIZE')
  oldused <- argnames %in% oldargs
  if (any(oldused))
    stop(sprintf('old style argument(s) specified: %s',
                 paste(argnames[oldused], collapse=', ')))

  .errorhandling <- match.arg(.errorhandling)

  combineInfo <- list(fun=.combine, in.order=.inorder, has.init=hasInit,
                      init=init, final=.final, multi.combine=.multicombine,
                      max.combine=.maxcombine)
  iterable <- list(args=args, argnames=argnames, evalenv=parent.frame(),
                   specified=specified, combineInfo=combineInfo,
                   errorHandling=.errorhandling, packages=.packages,
                   export=.export, noexport=.noexport, options=options,
                   verbose=.verbose)
  class(iterable) <- 'foreach'
  iterable
}

iter.foreach <- function(obj, ..., extra=list()) {
  # evaluate the quoted iteration variables, and turn them into iterators
  iargs <- lapply(obj$args, function(a) iter(eval(a, envir=extra,
                                                  enclos=obj$evalenv), ...))

  # create the environment that will contain our dynamic state
  state <- new.env(parent=emptyenv())

  # iterator state
  state$stopped <- FALSE
  state$numValues <- 0L  # number of values that we've fired

  # accumulator state
  combineInfo <- obj$combineInfo
  if (combineInfo$has.init) {
    state$accum <- eval(combineInfo$init, envir=extra, enclos=obj$evalenv)
    state$first.time <- FALSE
  } else {
    state$accum <- NULL
    state$first.time <- TRUE
  }
  state$fun <- combineInfo$fun
  state$buffered <- rep(as.integer(NA), 2 * combineInfo$max.combine)
  state$next.tag <- 1L  # only used when in.order is TRUE
  state$buf.off <- 0L   # only used when in.order is TRUE
  state$nbuf <- 0L      # only used when in.order is FALSE
  state$numResults <- 0L  # number of results that we've received back
  state$errorValue <- NULL
  state$errorIndex <- -1L

  # package and return the iterator object
  iterator <- list(state=state, iargs=iargs, argnames=obj$argnames,
                   combineInfo=combineInfo, errorHandling=obj$errorHandling,
                   verbose=obj$verbose)
  class(iterator) <- c('iforeach', 'iter')
  iterator
}

nextElem.iforeach <- function(obj, ..., redo=FALSE) {
  if (redo)
    obj$state$numValues <- obj$state$numValues - 1L

  tryCatch({
    # XXX this shouldn't be recomputed repeatedly
    ix <- which(!nzchar(obj$argnames))
    elem <- if (length(ix) > 0) {
      lapply(obj$iargs[ix], nextElem)
      ix <- which(nzchar(obj$argnames))
      if (length(ix) > 0)
        lapply(obj$iargs[ix], nextElem)
      else
        list()
    } else {
      lapply(obj$iargs, nextElem)
    }
  },
  error=function(e) {
    if (identical(conditionMessage(e), 'StopIteration')) {
      obj$state$stopped <- TRUE
      if (complete(obj))
        callCombine(obj, TRUE)
    }
    stop(e)
  })

  obj$state$numValues <- obj$state$numValues + 1L
  elem
}

# XXX make this a method?
complete <- function(obj) {
  stopifnot(class(obj)[1] == 'iforeach')

  if (obj$verbose)
    cat(sprintf('numValues: %d, numResults: %d, stopped: %s\n',
                obj$state$numValues, obj$state$numResults, obj$state$stopped))

  obj$state$stopped && obj$state$numResults == obj$state$numValues
}

accumulate.iforeach <- function(obj, result, tag, ...) {
  obj$state$numResults <- obj$state$numResults + 1L

  # we can't receive more results than the number of tasks that we've fired
  stopifnot(obj$state$numResults <= obj$state$numValues)

  if (inherits(result, 'error') && is.null(obj$state$errorValue) &&
      obj$errorHandling %in% c('stop', 'remove')) {
    if (obj$verbose)
      cat('accumulate got an error result\n')
    obj$state$errorValue <- result
    obj$state$errorIndex <- tag
  }

  # we can already tell what our status is going to be
  status <- complete(obj)

  # put the result in our buffer cache
  name <- paste('result', tag, sep='.')
  assign(name, result, obj$state, inherits=FALSE)
  ibuf <- if (obj$combineInfo$in.order) {
    tag - obj$state$buf.off
  } else {
    obj$state$nbuf <- obj$state$nbuf + 1L
  }

  # make sure we always have trailing NA's
  blen <- length(obj$state$buffered)
  while (ibuf >= blen) {
    length(obj$state$buffered) <- 2 * blen
    blen <- length(obj$state$buffered)
  }

  obj$state$buffered[ibuf] <-
    if (inherits(result, 'error') && obj$errorHandling %in% c('stop', 'remove'))
      -tag
    else
      tag

  # do any combining that needs to be done
  callCombine(obj, status)

  # return with apprpriate status
  if (obj$verbose)
    cat(sprintf('returning status %s\n', status))
  status
}

callCombine <- function(obj, status) {
  if (obj$combineInfo$in.order) {
    repeat {
      needed <- obj$combineInfo$max.combine
      if (!obj$state$first.time)
        needed <- needed - 1

      n <- which(is.na(obj$state$buffered))[1] - 1L
      stopifnot(!is.na(n))
      n <- min(n, needed)
      if (n == needed || (status && n > 0)) {
        # get the names of the objects to be combined
        ind <- 1:n

        # filter out any errors (if error handling isn't 'pass')
        b <- obj$state$buffered[ind]
        allsyms <- paste('result', abs(b), sep='.')
        args <- b[b > 0]
        args <- if (length(args) > 0)
          paste('result', args, sep='.')
        else
          character(0)

        # XXX these operations won't be efficient for small values of max.combine
        blen <- length(obj$state$buffered)
        obj$state$buffered <- obj$state$buffered[(n+1):blen]
        length(obj$state$buffered) <- blen  # XXX put this off?
        obj$state$buf.off <- obj$state$buf.off + n

        # create the call object to call the combine function
        callobj <- if (obj$state$first.time) {
          if (length(args) > 0) {
            if (obj$verbose)
              cat('first call to combine function\n')  # not always true
            obj$state$first.time <- FALSE

            if (length(args) > 1)
              as.call(lapply(c('fun', args), as.name))
            else
              as.name(args)  # this evaluates to the value of the result
          } else {
            if (obj$verbose)
              cat('not calling combine function due to errors\n')
            NULL
          }
        } else {
          if (length(args) > 0) {
            if (obj$verbose)
              cat('calling combine function\n')
            as.call(lapply(c('fun', 'accum', args), as.name))
          } else {
            if (obj$verbose)
              cat('not calling combine function due to errors\n')
            NULL
          }
        }

        # call the combine function
        if (!is.null(callobj)) {
          if (obj$verbose) {
            cat('evaluating call object to combine results:\n  ')
            print(callobj)
          }
          obj$state$accum <- eval(callobj, obj$state)
        }

        # remove objects from buffer cache that we just processed
        # and all error objects
        remove(list=allsyms, pos=obj$state)
      } else {
        break
      }
    }
  } else {
    needed <- obj$combineInfo$max.combine
    if (!obj$state$first.time)
      needed <- needed - 1
    stopifnot(obj$state$nbuf <= needed)

    # check if it's time to combine
    if (obj$state$nbuf == needed || (status && obj$state$nbuf > 0)) {
      # get the names of the objects to be combined
      ind <- 1:obj$state$nbuf

      # filter out any errors (if error handling isn't 'pass')
      b <- obj$state$buffered[ind]
      allsyms <- paste('result', abs(b), sep='.')
      args <- b[b > 0]
      args <- if (length(args) > 0)
        paste('result', args, sep='.')
      else
        character(0)

      obj$state$buffered[ind] <- as.integer(NA)
      obj$state$nbuf <- 0L

      # create the call object to call the combine function
      callobj <- if (obj$state$first.time) {
        if (length(args) > 0) {
          if (obj$verbose)
            cat('first call to combine function\n')
          obj$state$first.time <- FALSE

          if (length(args) > 1)
            as.call(lapply(c('fun', args), as.name))
          else
            as.name(args)  # this evaluates to the value of the result
        } else {
          if (obj$verbose)
            cat('not calling combine function due to errors\n')
          NULL
        }
      } else {
        if (length(args) > 0) {
          if (obj$verbose)
            cat('calling combine function\n')
          as.call(lapply(c('fun', 'accum', args), as.name))
        } else {
          if (obj$verbose)
            cat('not calling combine function due to errors\n')
          NULL
        }
      }

      # call the combine function
      if (!is.null(callobj)) {
        if (obj$verbose) {
          cat('evaluating call object to combine results:\n  ')
          print(callobj)
        }
        obj$state$accum <- eval(callobj, obj$state)
      }

      # remove objects from buffer cache that we just processed
      remove(list=allsyms, pos=obj$state)
    }
  }
}

getResult.iforeach <- function(obj, ...) {
  if (is.null(obj$combineInfo$final))
    obj$state$accum
  else
    obj$combineInfo$final(obj$state$accum)
}

getErrorValue.iforeach <- function(obj, ...) {
  obj$state$errorValue
}

getErrorIndex.iforeach <- function(obj, ...) {
  obj$state$errorIndex
}

'%:%' <- function(e1, e2) {
  if (!inherits(e1, 'foreach'))
    stop('"%:%" was passed an illegal right operand')

  if (inherits(e2, 'foreach'))
    makeMerged(e1, e2)
  else if (inherits(e2, 'foreachCondition'))
    makeFiltered(e1, e2)
  else
    stop('"%:%" was passed an illegal right operand')
}

makeMerged <- function(e1, e2) {
  specified <- union(e1$specified, e2$specified)
  argnames <- union(e1$argnames, e2$argnames)
  packages <- union(e1$packages, e2$packages)
  export <- union(e1$export, e2$export)
  noexport <- union(e1$noexport, e2$noexport)
  options <- c(e1$options, e2$options)
  iterable <- list(e1=e1, e2=e2, specified=specified, argnames=argnames,
                   packages=packages, export=export, noexport=noexport,
                   options=options)

  # this gives precedence to the outer foreach
  inherit <- c('errorHandling', 'verbose')
  iterable[inherit] <- e2[inherit]
  iterable[e1$specified] <- e1[e1$specified]

  class(iterable) <- c('xforeach', 'foreach')
  iterable
}

iter.xforeach <- function(obj, ...) {
  state <- new.env(parent=emptyenv())
  state$stopped <- FALSE
  state$fired <- integer(0)
  state$ie2 <- list()
  state$errorValue <- NULL
  state$errorIndex <- -1L
  ie1 <- iter(obj$e1, ...)
  iterator <- list(state=state, ie1=ie1, e2=obj$e2, argnames=obj$argnames,
                   errorHandling=obj$errorHandling, verbose=obj$verbose)
  class(iterator) <- c('ixforeach', 'iter')
  iterator
}

nextElem.ixforeach <- function(obj, ..., redo=FALSE) {
  if (obj$verbose)
    cat(sprintf('nextElem.ixforeach called with redo %s\n', redo))

  if (redo) {
    i <- length(obj$state$fired)
    if (obj$verbose) {
      cat('refiring iterator - fired was:\n')
      print(obj$state$fired)
    }
    obj$state$fired[i] <- obj$state$fired[i] - 1L
    if (obj$verbose) {
      cat('fired is now:\n')
      print(obj$state$fired)
    }
  }

  repeat {
    if (!exists('nextval', obj$state, inherits=FALSE)) {
      tryCatch({
        obj$state$nextval <- nextElem(obj$ie1)
      },
      error=function(e) {
        if (identical(conditionMessage(e), 'StopIteration'))
          obj$state$stopped <- TRUE
        stop(e)
      })

      obj$state$ie2 <- c(obj$state$ie2, list(iter(obj$e2, extra=obj$state$nextval)))
      obj$state$fired <- c(obj$state$fired, 0L)
    }

    tryCatch({
      i <- length(obj$state$fired)
      v2 <- nextElem(obj$state$ie2[[i]], redo=redo)
      obj$state$fired[i] <- obj$state$fired[i] + 1L
      break
    },
    error=function(e) {
      if (!identical(conditionMessage(e), 'StopIteration'))
        stop(e)

      remove('nextval', pos=obj$state)

      if (complete(obj$state$ie2[[i]])) {
        callCombine(obj$state$ie2[[i]], TRUE)

        if (is.null(obj$state$errorValue)) {
          obj$state$errorValue <- getErrorValue(obj$state$ie2[[i]])
          obj$state$errorIndex <- getErrorIndex(obj$state$ie2[[i]])
        }

        accum <- getResult(obj$state$ie2[[i]])
        if (obj$verbose) {
          cat('propagating accumulated result up to the next level from nextElem\n')
          print(accum)
        }
        accumulate(obj$ie1, accum, i)  # XXX error handling?
      }
    })
    redo <- FALSE
  }

  c(obj$state$nextval, v2)
}

accumulate.ixforeach <- function(obj, result, tag, ...) {
  if (obj$verbose) {
    cat(sprintf('accumulating result with tag %d\n', tag))
    cat('fired:\n')
    print(obj$state$fired)
  }

  s <- cumsum(obj$state$fired)
  j <- 1L
  while (tag > s[[j]])
    j <- j + 1L

  i <- if (j > 1)
    as.integer(tag) - s[[j - 1]]
  else
    as.integer(tag)

  ie2 <- obj$state$ie2[[j]]

  if (accumulate(ie2, result, i)) {
    if (is.null(obj$state$errorValue)) {
      obj$state$errorValue <- getErrorValue(ie2)
      obj$state$errorIndex <- getErrorIndex(ie2)
    }

    accum <- getResult(ie2)
    if (obj$verbose) {
      cat('propagating accumulated result up to the next level from accumulate\n')
      print(accum)
    }
    accumulate(obj$ie1, accum, j)  # XXX error handling?
  }
}

getResult.ixforeach <- function(obj, ...) {
  getResult(obj$ie1, ...)
}

getErrorValue.ixforeach <- function(obj, ...) {
  obj$state$errorValue
}

getErrorIndex.ixforeach <- function(obj, ...) {
  obj$state$errorIndex
}

'%if%' <- function(e1, cond) {
  stop('obsolete')
}

when <- function(cond) {
  obj <- list(qcond=substitute(cond), evalenv=parent.frame())
  class(obj) <- 'foreachCondition'
  obj
}

makeFiltered <- function(e1, cond) {
  iterable <- c(list(e1=e1), cond)
  inherit <- c('argnames', 'specified', 'errorHandling', 'packages',
               'export', 'noexport', 'options', 'verbose')
  iterable[inherit] <- e1[inherit]
  class(iterable) <- c('filteredforeach', 'foreach')
  iterable
}

iter.filteredforeach <- function(obj, ...) {
  ie1 <- iter(obj$e1, ...)
  iterator <- list(ie1=ie1, qcond=obj$qcond, evalenv=obj$evalenv,
                   argnames=obj$argnames, errorHandling=obj$errorHandling,
                   verbose=obj$verbose)
  class(iterator) <- c('ifilteredforeach', 'iter')
  iterator
}

nextElem.ifilteredforeach <- function(obj, ..., redo=FALSE) {
  repeat {
    elem <- nextElem(obj$ie1, ..., redo=redo)
    if (eval(obj$qcond, envir=elem, enclos=obj$evalenv))
      break
    redo <- TRUE
  }
  elem
}

accumulate.ifilteredforeach <- function(obj, result, tag, ...) {
  accumulate(obj$ie1, result, tag, ...)
}

getResult.ifilteredforeach <- function(obj, ...) {
  getResult(obj$ie1, ...)
}

getErrorValue.ifilteredforeach <- function(obj, ...) {
  getErrorValue(obj$ie1, ...)
}

getErrorIndex.ifilteredforeach <- function(obj, ...) {
  getErrorIndex(obj$ie1, ...)
}
