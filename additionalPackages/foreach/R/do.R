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

.foreachGlobals <- new.env(parent=emptyenv())

# this is called to register a parallel backend
setDoPar <- function(fun, data=NULL, info=function(data, item) NULL) {
  tryCatch(
    {
      assign('fun', fun, pos=.foreachGlobals, inherits=FALSE)
      assign('data', data, pos=.foreachGlobals, inherits=FALSE)
      assign('info', info, pos=.foreachGlobals, inherits=FALSE)
    }, error = function(e) {
         if (exists('fun', where=.foreachGlobals, inherits=FALSE))
		remove('fun', envir=.foreachGlobals)
         if (exists('data', where=.foreachGlobals, inherits=FALSE))
		remove('data', envir=.foreachGlobals)
         if (exists('info', where=.foreachGlobals, inherits=FALSE))
		remove('info', envir=.foreachGlobals)
         e
	})
}


# this is called to register a sequential backend
setDoSeq <- function(fun, data=NULL, info=function(data, item) NULL) {
  tryCatch(
    {
       assign('seqFun', fun, pos=.foreachGlobals, inherits=FALSE)
       assign('seqData', data, pos=.foreachGlobals, inherits=FALSE)
       assign('seqInfo', info, pos=.foreachGlobals, inherits=FALSE)
    }, error = function(e) {
         if (exists('fun', where=.foreachGlobals, inherits=FALSE))
		remove('fun', envir = .foreachGlobals)
         if (exists('data', where=.foreachGlobals, inherits=FALSE))
		remove('data', envir = .foreachGlobals)
         if (exists('info', where=.foreachGlobals, inherits=FALSE))
		remove('info', envir = .foreachGlobals)
         e
        })
}

# this explicitly registers a sequential backend for do and dopar.
registerDoSEQ <- function() {
  setDoPar(doSEQ, NULL, info)
  setDoSeq(doSEQ, NULL, info)
}

# passed to setDoPar via registerDoSEQ, and called by getDoSeqWorkers, etc
info <- function(data, item) {
  switch(item,
         workers=1L,
         name='doSEQ',
         version=packageDescription('foreach', fields='Version'),
         NULL)
}

# this returns a logical value indicating if a sequential backend
# has been registered or not
getDoSeqRegistered <- function() {
  exists('seqFun', where=.foreachGlobals, inherits=FALSE)
}

# this returns a logical value indicating if a parallel backend
# has been registered or not
getDoParRegistered <- function() {
  exists('fun', where=.foreachGlobals, inherits=FALSE)
}

# this returns the number of workers used by the currently registered
# sequential backend
getDoSeqWorkers <- function() {
  wc <- if (exists('seqInfo', where=.foreachGlobals, inherits=FALSE))
    .foreachGlobals$seqInfo(.foreachGlobals$seqData, 'workers')
  else
    NULL

  # interpret a NULL as a single worker, but the backend
  # can return NA without interference
  if (is.null(wc)) 1L else wc
}

# this returns the number of workers used by the currently registered
# parallel backend
getDoParWorkers <- function() {
  wc <- if (exists('info', where=.foreachGlobals, inherits=FALSE))
    .foreachGlobals$info(.foreachGlobals$data, 'workers')
  else
    NULL

  # interpret a NULL as a single worker, but the backend
  # can return NA without interference
  if (is.null(wc)) 1L else wc
}

# this returns the name of the currently registered sequential backend
getDoSeqName <- function() {
  if (exists('seqInfo', where=.foreachGlobals, inherits=FALSE))
    .foreachGlobals$seqInfo(.foreachGlobals$seqData, 'name')
  else
    NULL
}

# this returns the name of the currently registered parallel backend
getDoParName <- function() {
  if (exists('info', where=.foreachGlobals, inherits=FALSE))
    .foreachGlobals$info(.foreachGlobals$data, 'name')
  else
    NULL
}

# this returns the version of the currently registered sequential backend
getDoSeqVersion <- function() {
  if (exists('seqInfo', where=.foreachGlobals, inherits=FALSE))
    .foreachGlobals$seqInfo(.foreachGlobals$seqData, 'version')
  else
    NULL
}

# this returns the version of the currently registered parallel backend
getDoParVersion <- function() {
  if (exists('info', where=.foreachGlobals, inherits=FALSE))
    .foreachGlobals$info(.foreachGlobals$data, 'version')
  else
    NULL
}

# used internally to get the currently registered parallel backend
getDoSeq <- function() {
  if (exists('seqFun', where=.foreachGlobals, inherits=FALSE)) {
    list(fun=.foreachGlobals$seqFun, data=.foreachGlobals$seqdata)
  } else {
    list(fun=doSEQ, data=NULL)
  }
}

# used internally to get the currently registered parallel backend
getDoPar <- function() {
  if (exists('fun', where=.foreachGlobals, inherits=FALSE)) {
    list(fun=.foreachGlobals$fun, data=.foreachGlobals$data)
  } else {
    if (!exists('parWarningIssued', where=.foreachGlobals, inherits=FALSE)) {
      warning('executing %dopar% sequentially: no parallel backend registered',
              call.=FALSE)
      assign('parWarningIssued', TRUE, pos=.foreachGlobals, inherits=FALSE)
    }
    list(fun=doSEQ, data=NULL)
  }
}

'%do%' <- function(obj, ex) {
  e <- getDoSeq()
  e$fun(obj, substitute(ex), parent.frame(), e$data)
}

'%dopar%' <- function(obj, ex) {
  e <- getDoPar()
  e$fun(obj, substitute(ex), parent.frame(), e$data)
}

comp <- if (getRversion() < "2.13.0") {
  function(expr, ...) expr
} else {
  compiler::compile
}

doSEQ <- function(obj, expr, envir, data) {
  # note that the "data" argument isn't used
  if (!inherits(obj, 'foreach'))
    stop('obj must be a foreach object')

  it <- iter(obj)
  accumulator <- makeAccum(it)

  for (p in obj$packages)
    library(p, character.only=TRUE)

  # compile the expression if we're using R 2.13.0 or greater
  xpr <- comp(expr, env=envir, options=list(suppressUndefined=TRUE))

  i <- 1
  tryCatch({
    repeat {
      # get the next set of arguments
      args <- nextElem(it)
      if (obj$verbose) {
        cat(sprintf('evaluation # %d:\n', i))
        print(args)
      }

      # assign arguments to local environment
      for (a in names(args))
        assign(a, args[[a]], pos=envir, inherits=FALSE)

      # evaluate the expression
      r <- tryCatch(eval(xpr, envir=envir), error=function(e) e)
      if (obj$verbose) {
        cat('result of evaluating expression:\n')
        print(r)
      }

      # process the results
      tryCatch(accumulator(list(r), i), error=function(e) {
        cat('error calling combine function:\n')
        print(e)
        NULL
      })
      i <- i + 1
    }
  },
  error=function(e) {
    if (!identical(conditionMessage(e), 'StopIteration'))
      stop(simpleError(conditionMessage(e), expr))
  })

  errorValue <- getErrorValue(it)
  errorIndex <- getErrorIndex(it)

  if (identical(obj$errorHandling, 'stop') && !is.null(errorValue)) {
    msg <- sprintf('task %d failed - "%s"', errorIndex,
                   conditionMessage(errorValue))
    stop(simpleError(msg, call=expr))
  } else {
    getResult(it)
  }
}
