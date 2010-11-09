\name{varcov}
\Rdversion{1.1}
\alias{varcov}
\title{Calculate (co)variance matrix of an arf model.}
\description{Calculate (co)variance matrix of an arf model using Sandwich estimation.}
\usage{
varcov(arfmodel)
}
\arguments{
  \item{arfmodel}{A valid arf model object.}
}
\details{
Varcov needs first-order derivatives and residual matrices, if they do not exist, they are created automatically.
}
\value{
Returns an object of class "model" with the appropriate slots.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, 
}
\keyword{ models }
