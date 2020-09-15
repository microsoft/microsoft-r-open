\name{totalSystemMemory}
\alias{totalSystemMemory}
\title{
Obtain Total System Memory
}
\description{
Uses operating system tools to return total system memory.
}
\usage{
totalSystemMemory()
}
\details{
On Unix-alikes, checks the \samp{/proc/meminfo} file for the total memory. If the system call
returns an error, the function returns \code{NA}. On Windows
systems, uses \samp{wmic} to obtain the \samp{TotalVisibleMemorySize}.
}
\value{
A numeric value representing total system memory in kBytes, or \code{NA}.
}
\author{
Microsoft Corporation \email{mrspack@microsoft.com}
}


\examples{
totalSystemMemory()
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sysdata }

