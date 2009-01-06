\name{runModel}
\alias{runModel}

\title{ Fit an ARF model }
\description{
  Fits a sequence of ARF models and determines the best model based on the BIC.
}
\usage{
runModel(directory, sequence = c("sequential", "manual"), regionstofit = 1)
}

\arguments{
  \item{directory}{ The root directory of the data files (assumes that /data and /weights directories exist) }
  \item{sequence}{ How many regions to fit. If sequential a sequence from 1 to the value of  \code{regionstofit} is fitted. If manual, \code{regionstofit} is assumed to be a numeric vector of regions to fit. }
  \item{regionstofit}{ Either a numeric value of a numeric vector. See \code{sequence}  }
}
\details{
 
}
\value{
Returns an object of class 'sequence' with information on the fitted models.
  
}
\references{  }
\author{ Wouter Weeda }
\note{  

}
\seealso{ \code{\link{arfS4lite-package}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arf }
\keyword{ fmri }
