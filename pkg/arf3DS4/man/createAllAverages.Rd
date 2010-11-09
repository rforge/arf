\name{createAllAverages}
\Rdversion{1.1}
\alias{createAllAverages}
\title{Create Averages of runs for all conditions.}
\description{
CreateAllAverages creates averages of runs for all conditions in an arf-experiment. It calls \code{\link{createAverages}} for all known data-directories.
}
\usage{
createAllAverages(experiment = NULL)
}
\arguments{
\item{experiment}{An arf experiment (object of class "experiment",see \code{\linkS4class{experiment}}).}
}

\details{
When an arf experiment is loaded using \code{\link{loadExp}} createAllAverages can be called without passing an argument \code{createAllAverages()}.
}
\value{
Returns \code{TRUE} when no errors are encounterd.
}

\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\link{createAverages}},\code{\linkS4class{experiment}},\code{\link{loadExp}}
}
\keyword{ models }
