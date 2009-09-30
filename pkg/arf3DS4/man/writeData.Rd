\name{writeData}
\Rdversion{1.1}
\alias{writeData}
\title{Write NIFTI/ANALYZE data.}
\description{Write a datavector to a NIFTI/ANALYZE file.}
\usage{
writeData(headinf, datavec)
}
\arguments{
  \item{headinf}{An object of (at least) class "nifti.header".} 
  \item{datavec}{The datavector to be saved (must be of the correct length).}
}
\details{
\code{headinf} may also be an object of class "fmri.data" as this class extends the "nifti.header" class directly. 
}
\value{
Returns \code{TRUE} when successful.
}
\references{ See the Nifti homepage (\url{http://nifti.nimh.nih.gov/}) for more information on the nifti filetype.}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{fmri.data}}, \code{\linkS4class{nifti.header}}
}
\keyword{ models }
\keyword{ methods }

