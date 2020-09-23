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

icountn <- function(vn) {
  n <- length(vn)
  if (n == 0)
    stop('illegal zero length vector')

  icar <- icount(vn[n])
  if (n > 1) {
    icdr <- icountn(vn[-n])
    hasVal <- FALSE
    nextVal <- NULL
  }

  nextEl <- if (n == 1) {
    function() nextElem(icar)
  } else {
    function() {
      repeat {
        if (!hasVal) {
          nextVal <<- nextElem(icar)
          hasVal <<- TRUE
        }

        tryCatch({
          return(c(nextElem(icdr), nextVal))
        },
        error=function(e) {
          if (identical(conditionMessage(e), 'StopIteration')) {
            icdr <<- icountn(vn[-n])
            hasVal <<- FALSE
          } else {
            stop(e)
          }
        })
      }
    }
  }

  structure(list(nextElem=nextEl), class=c('abstractiter', 'iter'))
}

iwhich <- function(nf, ind) {
  n <- length(ind)
  if (n == 0)
    stop('illegal zero length vector')

  x <- rep(TRUE, length(nf[[1]]))
  for (i in seq_len(n))
    x <- x & nf[[i]] == ind[i]

  which(x)
}

# define the generic function
isplit <- function(x, f, drop=FALSE, ...) {
  UseMethod('isplit')
}

# define the default method
isplit.default <- function(x, f, drop=FALSE, ...) {
  if (!is.list(f)) f <- list(f)
  cf <- lapply(f, function(a) if (is.factor(a)) a else as.factor(a))
  nf <- lapply(cf, as.integer)
  flevels <- lapply(f, function(a) if (is.factor(a)) levels(a) else sort(unique.default(a)))
  it <- icountn(unlist(lapply(cf, nlevels)))

  nextEl <- function() {
    repeat {
      i <- nextElem(it)
      j <- iwhich(nf, i)
      if (!drop || length(j) > 0)
        break
    }
    k <- seq_along(i)
    names(k) <- names(cf)
    key <- lapply(k, function(x) flevels[[x]][i[x]])
    list(value=x[j], key=key)
  }

  structure(list(nextElem=nextEl), class=c('abstractiter', 'iter'))
}

# define the data frame method which uses the default method
isplit.data.frame <- function(x, f, drop=FALSE, ...) {
  it <- isplit(seq_len(nrow(x)), f, drop=drop, ...)
  nextEl <- function() {
    i <- nextElem(it)
    list(value=x[i$value,, drop=FALSE], key=i$key)
  }
  structure(list(nextElem=nextEl), class=c('abstractiter', 'iter'))
}
