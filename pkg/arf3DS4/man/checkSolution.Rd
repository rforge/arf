\name{checkSolution}
\Rdversion{1.1}
\alias{checkSolution}

\title{ check parameters at the boundaries }
\description{
Check the arf solution for parameters stuck at the boundaries (in constrained optimization) or over the boundaries (in unconstrained optimization)
}
\usage{
checkSolution(arfmodel, options = loadOptions(arfmodel), dat = readData(.model.avgdatfile(arfmodel)), thres = 6)
}
\arguments{
  \item{arfmodel}{An arf model object (see \code{\linkS4class{model}}).}
  \item{options}{The options object belonging to the model.}
  \item{dat}{The data to which the model was fitted.}
  \item{thres}{Number of digits in rounding for the equality check.}
}
\details{
Options and data are loaded by default from the directories specified in the modelobject
}
\value{
Returns an arf model object with warnings if there were paramters at the bound.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\linkS4class{options}}
}
\keyword{ models }
