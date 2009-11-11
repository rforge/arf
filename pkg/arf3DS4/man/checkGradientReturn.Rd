\name{checkGradientReturn}
\alias{checkGradientReturn}

\title{Check gradients of an arfmodel.}
\description{Check if the gradients of the parameters of an arfmodel are exceeding a threshold.}
\usage{
checkGradientReturn(arfmodel, absthres = 1000)}

\arguments{
 \item{arfmodel}{An arf model object (class \code{\linkS4class{model}})}
  \item{absthres}{Absolute threshold for gradient check.}
}
\value{
Returns a vector with the region numbers with parameters exceeding the threshold.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}
}
\keyword{ models }

