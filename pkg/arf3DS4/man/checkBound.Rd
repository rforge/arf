\name{checkBound}
\Rdversion{1.1}
\alias{checkBound}
\title{Check boundaries of parameter space.}
\description{
Checkbound checks if estimated paramters are at the boundaries set by optim.
}
\usage{
checkBound(arfmodel, lowbound, upbound)
}
\arguments{
  \item{arfmodel}{An arf model object.}
  \item{lowbound}{Vector with lower boundaries for each parameter.}
  \item{upbound}{Vector with upper boundaries for each parameter.}
}
\value{
Returns an object of class 'model'.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}
}
\keyword{ models }

