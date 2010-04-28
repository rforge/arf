\name{fmri2array}
\alias{fmri2array}

\title{Convert an fmridata object to an array}
\description{Convert the data vector of an fmri.data object to an array with correct dimensions (3D and 4D). }
\usage{
fmri2array(fmridat)
}
\arguments{
  \item{fmridat}{An object of class fmri.data.}
}
\value{Returns an array of dimensions set in the fmri.data object.}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{fmri.data}}
}
\keyword{ models }