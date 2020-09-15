\name{codetools}
\alias{codetools}
\title{Low Level Code Analysis Tools for R}
\usage{
collectLocals(e, collect) 
collectUsage(fun, name = "<anonymous>", ...) 
constantFold(e, env = NULL, fail = NULL) 
findFuncLocals(formals, body) 
findLocals(e, envir = .BaseEnv) 
findLocalsList(elist, envir = .BaseEnv)
flattenAssignment(e) 
getAssignedVar(e) 
isConstantValue(v, w) 
makeCodeWalker(..., handler, call, leaf) 
makeLocalsCollector(..., leaf, handler, isLocal, exit, collect) 
makeUsageCollector(fun, ..., name, enterLocal, enterGlobal, enterInternal,
                   startCollectLocals, finishCollectLocals, warn,
                   signal)
walkCode(e, w = makeCodeWalker()) 
}
\alias{collectLocals}
\alias{collectUsage}
\alias{constantFold}
\alias{findFuncLocals}
\alias{findLocals}
\alias{findLocalsList}
\alias{flattenAssignment}
\alias{getAssignedVar}
\alias{isConstantValue}
\alias{makeCodeWalker}
\alias{makeConstantFolder}
\alias{makeLocalsCollector}
\alias{makeUsageCollector}
\alias{walkCode}
\arguments{
  \item{e}{R expression.}
  \item{elist}{list of R expressions.}
  \item{v}{R object.}
  \item{fun}{closure.}
  \item{formals}{formal arguments of a closure.}
  \item{body}{body of a closure.}
  \item{name}{character.}
  \item{env}{character.}
  \item{envir}{environment.}
  \item{w}{code walker.}
  \item{\dots}{extra elements for code walker.}
  \item{collect}{function.}
  \item{fail}{function.}
  \item{handler}{function.}
  \item{call}{function.}
  \item{leaf}{function.}
  \item{isLocal}{function.}
  \item{exit}{function.}
  \item{enterLocal}{function.}
  \item{enterGlobal}{function.}
  \item{enterInternal}{function.}
  \item{startCollectLocals}{function.}
  \item{finishCollectLocals}{function.}
  \item{warn}{function.}
  \item{signal}{function.}
}
\description{
  These functions provide some tools for analysing R code.  Mainly
  intended to support the other tools in this package and byte code
  compilation.
}
\author{Luke Tierney}
\keyword{programming}
