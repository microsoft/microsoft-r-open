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

getsyms <- function(ex) {
  fun <- function(x) {
    if (is.symbol(x))
      as.character(x)
    else if (is.call(x))
      getsyms(x)
    else
      NULL
  }
  unlist(lapply(ex, fun))
}

gather <- function(x) {
  fun <- function(a, b) unique(c(a, b))
  accum <- list(good=character(0), bad=character(0))
  for (e in x) {
    accum <- mapply(fun, e, accum, SIMPLIFY=FALSE)
  }
  accum
}

expandsyms <- function(syms, env, good, bad) {
  fun <- function(sym, good, bad) {
    if (sym %in% c(good, bad)) {
      # we already saw this symbol
      list(good=good, bad=bad)
    } else if (!nzchar(sym)) {
      # apparently a symbol can be converted into an empty string,
      # but it's an error to call "exists" with an empty string,
      # so we just declare it to be bad here
      list(good=good, bad=c(sym, bad))
    } else if (exists(sym, env, mode='function', inherits=FALSE)) {
      # this is a function defined in this environment
      good <- c(sym, good)
      f <- get(sym, env, mode='function', inherits=FALSE)
      if (identical(environment(f), env)) {
        # it's a local function
        globs <- findGlobals(f)
        if (length(globs) > 0) {
          # it's got free variables, so let's check them out
          gather(lapply(globs, fun, good, bad))
        } else {
          # it doesn't have free variables, so we're done
          list(good=good, bad=bad)
        }
      } else {
        # it's not a local function, so we're done
        list(good=good, bad=bad)
      }
    } else if (exists(sym, env, inherits=FALSE)) {
      # it's not a function, but it's defined in this environment
      list(good=c(sym, good), bad=bad)
    } else {
      # it's not defined in this environment
      list(good=good, bad=c(sym, bad))
    }
  }
  gather(lapply(syms, fun, good, bad))$good
}

getexports <- function(ex, e, env, good=character(0), bad=character(0)) {
  syms <- getsyms(ex)
  useFuture <- FALSE
  if (requireNamespace("future", quietly=TRUE) && !identical(getOption("foreachGlobals"), "foreach")){
    useFuture <- TRUE
    gp <- future::getGlobalsAndPackages(ex,env)
	ngp <- names(gp$globals)
	syms <- union(expandsyms(syms, env, good, bad), ngp)
	packages <- gp$packages
  } else {
	packages <- NULL
	syms <- expandsyms(syms, env, good, bad)
  }
  for (s in syms) {
    if (s != '...') {
	if (!useFuture){
      val <- get(s, env, inherits=FALSE)
	} else {
	  if (s %in% ngp){
		val <- gp$globals[[match(s,ngp)]]
	  } else {
		val <- get(s, env, inherits=FALSE)
	  }
	} 

      # if this is a function, check if we should change the
      # enclosing environment to be this new environment
      fenv <- environment(val)
      if (is.function(val) &&
          (identical(fenv, env) || identical(fenv, .GlobalEnv)))
        environment(val) <- e

      assign(s, val, e)
    }
  }


  invisible(packages)
}
