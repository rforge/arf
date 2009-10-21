\name{validStart}
\Rdversion{1.1}
\alias{validStart}
\title{ check the validity of starting values }
\description{
Check the validity of startting values for an arf model. It checks for improper starting values and for unlikely starting values.
}
\usage{
validStart(arfmodel)
}

\arguments{
  \item{arfmodel}{An arf model object (see \code{\linkS4class{model}}).}
}
\details{
Will set the model to invalid if there are improper values, will warn with unlikely values.
}
\value{
Returns an arf model object with warnings if there were paramters at the bound.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}
}
\keyword{ models }

