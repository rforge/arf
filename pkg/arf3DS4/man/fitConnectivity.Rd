\name{fitConnectivity}
\alias{fitConnectivity}

\title{
Estimate connectivity of an ARF model
}
\description{
fitConnectivity estimates the functional connectivity between activated regions using a 'single-event' volume.
}
\usage{
fitConnectivity(arfmodel, fname = "single_events")
}

\arguments{
  \item{arfmodel}{An arf model object (class \code{\linkS4class{model}})}
  \item{fname}{Filename of the volume to extract the time-series.}
}
\details{
Each volume of the file specified in fname should contain estimates of single-trial events (see \code{\link{makeSingleTrialEvents}}).
}
\value{
Returns a region by time matrix of amplitude time-series.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\link{makeSingleTrialEvents}}
}
\keyword{ models }
