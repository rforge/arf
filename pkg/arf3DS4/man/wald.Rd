\name{wald}
\Rdversion{1.1}
\alias{wald}
\title{Calculate Wald statistics for an arf model.}
\description{Calculate Wald statistics for an arf model.}
\usage{
wald(arfmodel, waldobject = new("wald"), options = loadOptions(arfmodel))
}
\arguments{
  \item{arfmodel}{A valid model object.}
  \item{waldobject}{An object of class "wald".}
  \item{options}{An object of class "options".}
}
\details{
Wald requires a (co)variance matrix, it will give an error when the \code{model@varcov} slot does not exist. 
}
\value{
Returns an object of class "model" with the appropriate slots.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\link{varcov}}, \code{\linkS4class{wald}}, \code{\linkS4class{options}}
}
\keyword{ models }
