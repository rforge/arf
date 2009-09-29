\name{determineStartRect}
\Rdversion{1.1}
\alias{determineStartRect}
\alias{determineStartRectSimple}

\title{ Calculate starting values for an arf model}
\description{
determineStartRect calculates starting values for an arf model by searching for maxima and minima in the data (averaged t-stat image).
}
\usage{
determineStartRect(arfmodel, options = loadOptions(arfmodel))
determineStartRectSimple(arfmodel, options = loadOptions(arfmodel))
}
\arguments{
  \item{arfmodel}{An arf model object (class \code{\linkS4class{model}})}
  \item{options}{An "options" object (class \code{\linkS4class{options}}), by default the options.Rda from the current model is loaded. The value of \code{options@start.maxfac} determines the calculation of the spatial extent values, higher settings will lead to greater extent values.}
}
\details{
determineStartRect determines the maxima and minima of the data and at each maximum/minimum searches parallel to each dimension (x,y,z) where the maximum/minimum value is half its original value. This creates a box around the maximum/minimum, starting values are taken from the box and it is then set to zero.
}
\value{
Returns the arf "model" object, with the starting values in the \code{startval} slot.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\linkS4class{options}}
}
\keyword{ models }
