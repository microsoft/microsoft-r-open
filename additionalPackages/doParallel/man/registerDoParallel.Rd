\name{registerDoParallel}
\alias{registerDoParallel}
\alias{stopImplicitCluster}
\title{registerDoParallel}
\description{
The \code{registerDoParallel} function is used to register the 
parallel backend with the \code{foreach} package.
}
\usage{
registerDoParallel(cl, cores=NULL, \dots)
stopImplicitCluster()
}
\arguments{
  \item{cl}{A cluster object as returned by \code{makeCluster}, or the number
  of nodes to be created in the cluster. If not specified, on Windows a 
  three worker cluster is created and used.}
  \item{cores}{The number of cores to use for parallel execution. If not
  specified, the number of cores is set to the value of 
  \code{options("cores")}, if specified, or to one-half the number of cores detected
  by the \code{parallel} package.}
  \item{\dots}{Package options.  Currently, only the \code{nocompile} option
  is supported.  If \code{nocompile} is set to \code{TRUE}, compiler
  support is disabled.}
}
\details{
The \code{parallel} package from R 2.14.0 and later provides functions for 
parallel execution of R code on machines with multiple cores or processors 
or multiple computers.  It is essentially a blend of the \code{snow} and 
\code{multicore} packages. By default, the \code{doParallel} package uses 
\code{snow}-like functionality.  The \code{snow}-like functionality
should work fine on Unix-like systems, but the \code{multicore}-like 
functionality is limited to a single sequential worker on Windows systems.
On workstations with multiple cores running Unix-like operating systems, 
the system \code{fork} call is used to spawn copies of the current process. 

The \code{doParallel} backend supports both multicore and snow options passed
through the \code{foreach} function. 
The supported multicore options are \code{preschedule}, \code{set.seed},
\code{silent}, and \code{cores}, which are analogous to the similarly named 
arguments to \code{\link{mclapply}}, and are passed using the
\code{.options.multicore} argument to \code{foreach}. The supported snow options are 
\code{preschedule}, which like its multicore analog can be used to chunk the 
tasks so that each worker gets a prescheduled chunk of tasks, and 
\code{attachExportEnv}, which can be used to attach the export environment
in certain cases where R's lexical scoping is unable to find a needed
export. The snow options are passed to \code{foreach} using the \code{.options.snow}
argument.

The function \code{stopImplicitCluster} can be used in vignettes and other places
where it is important to explicitly close the implicitly created cluster.
}
\examples{
cl <- makePSOCKcluster(2)
registerDoParallel(cl)
m <- matrix(rnorm(9), 3, 3)
foreach(i=1:nrow(m), .combine=rbind) %dopar%  (m[i,] / mean(m[i,]))
stopCluster(cl)
}
\keyword{utilities}
