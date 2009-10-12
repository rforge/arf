\name{makeDerivs}
\Rdversion{1.1}
\alias{makeDerivs}
\title{ Calculate analytical first-order derivatives of an arf model.}
\description{
makeDerivs calculates analytical first-order derivatives of an arf model. It writes the derivative matrix to a binary file.
}
\usage{
makeDerivs(arfmodel)
}
\arguments{
  \item{arfmodel}{An arf model object (see \code{\linkS4class{model}}).}
}
\value{
Returns \code{TRUE} when successful.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}
}
\keyword{ models }
