\name{checkSolutionReturn}
\alias{checkSolutionReturn}
\title{Check parameter boundaries of an arfmodel}
\description{Check if parameters of an arfmodel are stuck at a boundary.}
\usage{
checkSolutionReturn(arfmodel, options = loadOptions(arfmodel), dat = readData(.model.avgdatfile(arfmodel)), thres = 8)
}

\arguments{
 	\item{arfmodel}{An arf model object (class \code{\linkS4class{model}})}
   	\item{options}{An "options" object (class \code{\linkS4class{options}})}
  	\item{dat}{An object of class "fmri.data" (\code{\linkS4class{fmri.data}} containing the beta-values.)}
  	\item{thres}{Number of digits used in equality check on the bound.}
}

\value{
Returns a vector with the region numbers with parameters on the bound.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\linkS4class{options}}, \code{\linkS4class{fmri.data}}
}
\keyword{ models }
