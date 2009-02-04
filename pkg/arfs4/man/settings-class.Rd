\name{settings-class}
\docType{class}
\alias{settings-class}


\title{Class "settings"}
\description{	Settings contains global information used by ARF (especially about starting value calculation and range checks).}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("settings", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{min.iterlim}:}{Maximum number of iterations used by the minimzation procedure }
    \item{\code{start.method}:}{Which method is used to calculate the starting values (default is 'rect') }
    \item{\code{start.maxfac}:}{Factor used in determining starting values of the extent parameters (default is 2) }
    \item{\code{start.vector}:}{Vector of starting values (mainly used to set starting values for parameter 5 and 6)}
    \item{\code{chk.method}:}{Which method is used to check if the solution is sensible (i.e. location falls within the map, parameter 5 not close to 1) }
    \item{\code{chk.range}:}{Range of valid values for parameters.}
    \item{\code{warn}:}{Suppress warnings? }
    \item{\code{sw.type}:}{Which sandwich method to use: 'diag','tria','full', or 'file', (default='file') }

  }
}
\section{Methods}{
  \describe{
    \item{slots:}{Slots can be accessed via the standard accessor and replacment methods.}

	 }
}

\author{ Wouter D. Weeda }
\note{ A new 'settings' object is made in the call to \code{\link{arf}} if it does not already exist in .GlobalEnv }

\keyword{classes}
