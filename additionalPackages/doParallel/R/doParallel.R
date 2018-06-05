#
# Copyright (c) 2008-2010, Revolution Analytics
#
# This program is free software; you can redistribute it and/or modify 
# it under the terms of the GNU General Public License (version 2) as 
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
# General Public License for more details.
#
# A copy of the GNU General Public License is available at
# http://www.r-project.org/Licenses/
#

.options <- new.env(parent=emptyenv())
.revoDoParCluster <- NULL

# this explicitly registers a multicore parallel backend
registerDoParallel <- function(cl, cores=NULL, ...) {
  opts <- list(...)
  optnames <- names(opts)
  if (is.null(optnames))
    optnames <- rep('', length(opts))

  # filter out unnamed arguments with a warning
  unnamed <- ! nzchar(optnames)
  if (any(unnamed)) {
    warning('ignoring doParallel package option(s) specified with unnamed argument')
    opts <- opts[!unnamed]
    optnames <- optnames[!unnamed]
  }

  # filter out unrecognized options with a warning
  recog <- optnames %in% c('nocompile')
  if (any(!recog)) {
    warning(sprintf('ignoring unrecognized doParallel package option(s): %s',
	  			  paste(optnames[!recog], collapse=', ')), call.=FALSE)
    opts <- opts[recog]
    optnames <- optnames[recog]
  }

  # clear .options in case registerDoParallel is called multiple times
  old.optnames <- ls(.options, all.names=TRUE)
  rm(list=old.optnames, pos=.options)

  # set new options
  for (i in seq(along=opts)) {
    assign(optnames[i], opts[[i]], pos=.options)
  }

  if (missing(cl) || is.numeric(cl)) {
    if (.Platform$OS.type == "windows") {
	  if (!missing(cl) && is.numeric(cl)) {
            cl <- makeCluster(cl)
	  } else {
        if (!missing(cores) && is.numeric(cores)){
            cl <- makeCluster(cores)
        } else {
            cl <- makeCluster(3)
        }
	  }
	  assign(".revoDoParCluster", cl, pos=.options)
	  reg.finalizer(.options, function(e){
	      stopImplicitCluster()
		}, onexit = TRUE)
	  setDoPar(doParallelSNOW, cl, snowinfo)
    } else {
      if (!missing(cl) && is.numeric(cl)) {
        cores <- cl
      }
      # register multicore backend
      setDoPar(doParallelMC, cores, mcinfo) 
    }
  } else {
    setDoPar(doParallelSNOW, cl, snowinfo)
  }
}

"stopImplicitCluster" <- function()
{
    if (exists(".revoDoParCluster", where=.options) && !is.null(.options[['.revoDoParCluster']])) {
        stopCluster(.options[['.revoDoParCluster']])
        remove(".revoDoParCluster", envir=.options)
    }
}

# internal function that determines the number of workers to use
workers <- function(data) {
  if ("cluster" %in% class(data)) {
    length(data)
  } else {
    cores <- data
    if (!is.null(cores)) {
      # use the number specified when registering doMC
      cores
    } else {
      cores <- getOption('cores')
      if (!is.null(cores)) {
        # use the number specified via the 'cores' option
        cores
      } else {
        # use 1/2 the number detected by parallel 
		cores <- parallel::detectCores()
		if (cores > 2) {
		  cores <- ceiling(cores/2)
		}
		cores
	  }
	}
  }
}

# passed to setDoPar via registerDoParallel, and called by getDoParWorkers, etc
mcinfo <- function(data, item) {
  switch(item,
         workers=workers(data),
         name='doParallelMC',
         version=packageDescription('doParallel', fields='Version'),
         NULL)
}

# passed to setDoPar via registerDoParallel, and called by getDoParWorkers, etc
snowinfo <- function(data, item) {
  switch(item,
         workers=workers(data),
         name='doParallelSNOW',
         version=packageDescription('doParallel', fields='Version'),
         NULL)
}

comp <- if (getRversion() < "2.13.0") {
  function(expr, ...) expr
} else {
  function(expr, ...) {
    if (isTRUE(.options$nocompile))
      expr
    else
      compiler::compile(expr, ...)
  }
}


parSpl <- try(parallel::splitList, silent=TRUE)
## Use the "splitList" function from parallel if it's exported
## Otherwise, use the definition it had in R 3.0.2.
"splitList" <- if (inherits(parSpl, "try-error")) {
    function (x, ncl) 
	lapply(splitIndices(length(x), ncl), function(i) x[i])
} else {
	parSpl
}

doParallelMC <- function(obj, expr, envir, data) {
  # set the default mclapply options
  preschedule <- TRUE
  set.seed <- TRUE
  silent <- FALSE
  cores <- workers(data)

  if (!inherits(obj, 'foreach'))
    stop('obj must be a foreach object')

  it <- iter(obj)
  argsList <- as.list(it)
  accumulator <- makeAccum(it)

  # make sure all of the necessary libraries have been loaded
  for (p in obj$packages)
    library(p, character.only=TRUE)

  # check for multicore-specific options
  options <- obj$options$multicore
  if (!is.null(options)) {
    nms <- names(options)
    recog <- nms %in% c('preschedule', 'set.seed', 'silent', 'cores')
    if (any(!recog))
      warning(sprintf('ignoring unrecognized multicore option(s): %s',
                      paste(nms[!recog], collapse=', ')), call.=FALSE)

    if (!is.null(options$preschedule)) {
      if (!is.logical(options$preschedule) || length(options$preschedule) != 1) {
        warning('preschedule must be logical value', call.=FALSE)
      } else {
        if (obj$verbose)
          cat(sprintf('setting mc.preschedule option to %d\n', options$preschedule))
        preschedule <- options$preschedule
      }
    }

    if (!is.null(options$set.seed)) {
      if (!is.logical(options$set.seed) || length(options$set.seed) != 1) {
        warning('set.seed must be logical value', call.=FALSE)
      } else {
        if (obj$verbose)
          cat(sprintf('setting mc.set.seed option to %d\n', options$set.seed))
        set.seed <- options$set.seed
      }
    }

    if (!is.null(options$silent)) {
      if (!is.logical(options$silent) || length(options$silent) != 1) {
        warning('silent must be logical value', call.=FALSE)
      } else {
        if (obj$verbose)
          cat(sprintf('setting mc.silent option to %d\n', options$silent))
        silent <- options$silent
      }
    }

    if (!is.null(options$cores)) {
      if (!is.numeric(options$cores) || length(options$cores) != 1 ||
          options$cores < 1) {
        warning('cores must be numeric value >= 1', call.=FALSE)
      } else {
        if (obj$verbose)
          cat(sprintf('setting mc.cores option to %d\n', options$cores))
        cores <- options$cores
      }
    }
  }

  # define the "worker" function, compiling expr if possible
  c.expr <- comp(expr, env=envir, options=list(suppressUndefined=TRUE))
  FUN <- function(args) tryCatch(eval(c.expr, envir=args, enclos=envir),
                                 error=function(e) e)

  # execute the tasks
  results <- mclapply(argsList, FUN, mc.preschedule=preschedule,
                      mc.set.seed=set.seed, mc.silent=silent,
                      mc.cores=cores)

  # call the accumulator with all of the results
  tryCatch(accumulator(results, seq(along=results)), error=function(e) {
    cat('error calling combine function:\n')
    print(e)
    NULL
  })

  # check for errors
  errorValue <- getErrorValue(it)
  errorIndex <- getErrorIndex(it)

  # throw an error or return the combined results
  if (identical(obj$errorHandling, 'stop') && !is.null(errorValue)) {
    msg <- sprintf('task %d failed - "%s"', errorIndex,
                   conditionMessage(errorValue))
    stop(simpleError(msg, call=expr))
  } else {
    getResult(it)
  }
}

makeDotsEnv <- function(...) {
  list(...)
  function() NULL
}

.doSnowGlobals <- new.env(parent=emptyenv())

getparentenv <- function(pkgname) {
  parenv <- NULL

  # if anything goes wrong, print the error object and return
  # the global environment
  tryCatch({
    # pkgname is NULL in many cases, as when the foreach loop
    # is executed interactively or in an R script
    if (is.character(pkgname)) {
      # load the specified package
      if (require(pkgname, character.only=TRUE)) {
        # search for any function in the package
        pkgenv <- as.environment(paste0('package:', pkgname))
        for (sym in ls(pkgenv)) {
          fun <- get(sym, pkgenv, inherits=FALSE)
          if (is.function(fun)) {
            env <- environment(fun)
            if (is.environment(env)) {
              parenv <- env
              break
            }
          }
        }
        if (is.null(parenv)) {
          stop('loaded ', pkgname, ', but parent search failed', call.=FALSE)
        } else {
          message('loaded ', pkgname, ' and set parent environment')
        }
      }
    }
  },
  error=function(e) {
    cat(sprintf('Error getting parent environment: %s\n',
                conditionMessage(e)))
  })

  # return the global environment by default
  if (is.null(parenv)) globalenv() else parenv
}

workerInit <- function(expr, exportenv, pkgname, packages, attach=FALSE) {
  assign('expr', expr, .doSnowGlobals)
  assign('exportenv', exportenv, .doSnowGlobals)
  exportEnv <- .doSnowGlobals$exportenv
  parent.env(exportEnv) <- getparentenv(pkgname)
  if (attach) {
    attach(exportEnv)
  }
  
  tryCatch({
    for (p in packages)
      library(p, character.only=TRUE)

    NULL  # indicates success
  },
  error=function(e) {
    # a character string indicates an error
    conditionMessage(e)
  })
}
workerCleanup <- function() {
	if ("exportEnv" %in% search()) {
		detach(exportEnv)
	}
}

evalWrapper <- function(args) {
  lapply(names(args), function(n) assign(n, args[[n]], pos=.doSnowGlobals$exportenv))
  tryCatch(eval(.doSnowGlobals$expr, envir=.doSnowGlobals$exportenv), error=function(e) e)
}

# This function takes the place of workerInit and evalWrapper when
# preschedule is enabled.  It is executed by the master via clusterApply
# such that there is a single chunked task for each worker in the
# cluster, rather than using clusterCall to initialize the workers and
# clusterApplyLB to compute the tasks one-by-one.  This strategy can be
# significantly more efficient when there are many small tasks, and is
# very similar to the default behavior of mclapply.
workerPreschedule <- function(largs, expr, exportenv, pkgname, packages) {
  parent.env(exportenv) <- getparentenv(pkgname)
  task <- function(args) {
    lapply(names(args), function(n) assign(n, args[[n]], pos=exportenv))
    eval(expr, envir=exportenv)
  }

  tryCatch({
    # load all necessary packages
    for (p in packages)
      library(p, character.only=TRUE)

    # execute all of the tasks
    lapply(largs, task)
  },
  error=function(e) {
    # only one exception was thrown, but we don't know which one,
    # so we'll return it for all of the tasks
    lapply(seq_along(largs), function(i) e)
  })
}

doParallelSNOW <- function(obj, expr, envir, data) {
  cl <- data
  preschedule <- FALSE
  attachExportEnv <- FALSE

  if (!inherits(obj, 'foreach'))
    stop('obj must be a foreach object')

  it <- iter(obj)
  accumulator <- makeAccum(it)

  # check for snow-specific options
  options <- obj$options$snow
  if (!is.null(options)) {
    nms <- names(options)
    recog <- nms %in% c('preschedule', 'attachExportEnv')
    if (any(!recog))
      warning(sprintf('ignoring unrecognized snow option(s): %s',
                      paste(nms[!recog], collapse=', ')), call.=FALSE)

    if (!is.null(options$preschedule)) {
      if (!is.logical(options$preschedule) ||
          length(options$preschedule) != 1) {
        warning('preschedule must be logical value', call.=FALSE)
      } else {
        if (obj$verbose)
          cat(sprintf('bundling all tasks into %d chunks\n', length(cl)))
        preschedule <- options$preschedule
      }
    }
    if (!is.null(options$attachExportEnv)) {
      if (!is.logical(options$attachExportEnv) ||
          length(options$attachExportEnv) != 1) {
        warning('attachExportEnv must be logical value', call.=FALSE)
      } else {
        if (obj$verbose)
          cat("attaching export environment\n")
        attachExportEnv <- options$attachExportEnv
      }
    }
  }

  # setup the parent environment by first attempting to create an environment
  # that has '...' defined in it with the appropriate values
  exportenv <- tryCatch({
    qargs <- quote(list(...))
    args <- eval(qargs, envir)
    environment(do.call(makeDotsEnv, args))
  },
  error=function(e) {
    new.env(parent=emptyenv())
  })
  noexport <- union(obj$noexport, obj$argnames)
  packages <- getexports(expr, exportenv, envir, bad=noexport)
  if(obj$verbose)
	cat(sprintf('discovered package(s): %s\n',
                  paste(packages, collapse=', ')))
  
  vars <- ls(exportenv)
  if (obj$verbose) {
    if (length(vars) > 0) {
      cat('automatically exporting the following variables',
          'from the local environment:\n')
      cat(' ', paste(vars, collapse=', '), '\n')
    } else {
      cat('no variables are automatically exported\n')
    }
  }

  # compute list of variables to export
  export <- unique(obj$export)
  ignore <- intersect(export, vars)
  if (length(ignore) > 0) {
    warning(sprintf('already exporting variable(s): %s',
            paste(ignore, collapse=', ')))
    export <- setdiff(export, ignore)
  }

  # add explicitly exported variables to exportenv
  if (length(export) > 0) {
    if (obj$verbose)
      cat(sprintf('explicitly exporting variables(s): %s\n',
                  paste(export, collapse=', ')))

    for (sym in export) {
      if (!exists(sym, envir, inherits=TRUE))
        stop(sprintf('unable to find variable "%s"', sym))
      val <- get(sym, envir, inherits=TRUE)
      if (is.function(val) &&
          (identical(environment(val), .GlobalEnv) ||
           identical(environment(val), envir))) {
        # Changing this function's environment to exportenv allows it to
        # access/execute any other functions defined in exportenv.  This
        # has always been done for auto-exported functions, and not
        # doing so for explicitly exported functions results in
        # functions defined in exportenv that can't call each other.
        environment(val) <- exportenv
      }
      assign(sym, val, pos=exportenv, inherits=FALSE)
    }
  }

  # send exports to workers
  c.expr <- comp(expr, env=envir, options=list(suppressUndefined=TRUE))
  
   # packageName function added in R 3.0.0
   pkgname <- if (exists('packageName', mode='function'))
     packageName(envir)
   else
     NULL

  packages = c(packages, obj$packages)
  if (obj$verbose) {
	cat(sprintf('explicitly exporting package(s): %s\n',
                  paste(packages, collapse=', ')))
  }
  if (! preschedule) {
    # send exports to workers
    r <- clusterCall(cl, workerInit, c.expr, exportenv, pkgname, 
                     packages, attachExportEnv)
    for (emsg in r) {
      if (!is.null(emsg))
        stop('worker initialization failed: ', emsg)
    }

    # execute the tasks
    argsList <- as.list(it)
    results <- clusterApplyLB(cl, argsList, evalWrapper)
	
	# clean up the workers
	if (attachExportEnv){
	  clusterCall(cl, workerCleanup)
	}
  } else {
    # convert argument iterator into a list of lists
    argsList <- splitList(as.list(it), length(cl))

    # execute the tasks
    results <- do.call(c, clusterApply(cl, argsList, workerPreschedule,
                                       c.expr, exportenv, pkgname, 
                                       packages))
  }


  # call the accumulator with all of the results
  tryCatch(accumulator(results, seq(along=results)), error=function(e) {
    cat('error calling combine function:\n')
    print(e)
  })

  # check for errors
  errorValue <- getErrorValue(it)
  errorIndex <- getErrorIndex(it)

  # throw an error or return the combined results
  if (identical(obj$errorHandling, 'stop') && !is.null(errorValue)) {
    msg <- sprintf('task %d failed - "%s"', errorIndex,
                   conditionMessage(errorValue))
    stop(simpleError(msg, call=expr))
  } else {
    getResult(it)
  }
}
