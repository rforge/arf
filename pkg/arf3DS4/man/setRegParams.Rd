\name{setRegParams}
\Rdversion{1.1}
\alias{setRegParams}
\title{Set the registration parameters.}
\description{Set the registration matrices to the appropriate values given the files in the registration directory.
}
\usage{
setRegParams(registration)
}
\arguments{
  \item{registration}{Object of class "registration" with appropriate filenames.}
}
\details{
First run \code{\link{setRegFiles}} to set the correct filenames.
}
\value{
Returns a registration object with the correct slots.
}
\references{
See \url{http://www.fmrib.ox.ac.uk/fsl/} on how to obtain FSL and licences.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{registration}}, \code{\link{setRegFiles}}
}
\keyword{ methods }
\keyword{ models }
