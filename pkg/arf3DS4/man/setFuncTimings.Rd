\name{setFuncTimings}
\Rdversion{1.1}
\alias{setFuncTimings}
\title{Set the timings of the stimuli and link to raw time-series}
\description{
Set the timings of the stimuli and link to raw time-series
}
\usage{
setFuncTimings(subject, condition, run, timings, func_data = NULL, experiment = NULL)
}
\arguments{
  \item{subject}{Subject}
  \item{condition}{Condition}
  \item{run}{Which run, can be numeric or character}
  \item{timings}{A vector with timings (in seconds), possibly with an attribute \code{stimlen}, a vector of stimulus lengths.}
  \item{func_data}{Name of the raw time-series datafile (in funcs dir)}
  \item{experiment}{.experiment}
}
\value{
Returns an object of class \code{\linkS4class{functional}}
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{functional}}, \code{\linkS4class{experiment}}
}
\keyword{ methods }
\keyword{ models }
