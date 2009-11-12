\name{makeROImask}
\alias{makeROImask}
\title{ Mask an fmri image}
\description{
MakeROImask masks an fmri image based on the non-zero elements of a maskfile (of class \code{\linkS4class{fmri.data}}).
}
\usage{
makeROImask(fmridata, maskdata)
}

\arguments{
  \item{fmridata}{The data to be masked (class \code{\linkS4class{fmri.data}}).}
  \item{maskdata}{The mask to be applied (uses non-zero voxels to mask, of class \code{\linkS4class{fmri.data}} or a numeric vector.)}
}
\value{Returns an masked object of class fmri.data)
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{fmri.data}}
}
\keyword{ models }