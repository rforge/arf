\name{createAverages}
\Rdversion{1.1}
\alias{createAverages}
\title{Create averages of runs for a condition.}
\description{
CreateAverages creates averages of runs for a condition in an arf-experiment.
}
\usage{
createAverages(arfdat, experiment = NULL)
}
\arguments{
  \item{arfdat}{An object of class "data" (\code{\linkS4class{data}}) from a condition.}
  \item{experiment}{An arf experiment (object of class "experiment",see \code{\linkS4class{experiment}}).}
}
\details{
When an arf experiment is loaded using \code{\link{loadExp}} createAverages can be called with only passing the \code{arfdat} object.
}
\value{
Returns the "data" object with the links to the average files in the slots \code{data@avgdatfile}, \code{data@avgWfile}, and \code{data@avgtstatfile}
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{data}},\code{\linkS4class{experiment}},\code{\link{loadExp}},\code{\linkS4class{data}}
}
\keyword{ models }
