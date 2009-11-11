\name{checkNonSigReturn}
\alias{checkNonSigReturn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check for non-significant regions.}
\description{Check which regions have non-significant spatial extents.}
\usage{checkNonSigReturn(arfmodel, alpha = 0.05)}
\arguments{
  \item{arfmodel}{An arf model object (class \code{\linkS4class{model}})}
  \item{alpha}{Alpha level to test for significance.}
}
\value{
Returns a vector with the region numbers with parameters exceeding the threshold.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}
}
\keyword{ models }

