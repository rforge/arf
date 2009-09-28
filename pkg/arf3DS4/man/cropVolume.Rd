\name{cropVolume}
\Rdversion{1.1}
\alias{cropVolume}
\title{
Crop a 3 dimensional fMRI volume to specified dimensions.
}
\description{
cropVolume crops a fMRI volume to specified dimensions and saves (overwrites!) the cropped fMRI volume.
}
\usage{
cropVolume(filename, resizeToDim, quiet = F)
}
\arguments{
  \item{filename}{Name of the fmri volume to be cropped (NIFTI/ANALYZE format).}
  \item{resizeToDim}{Dimensions to crop the volume to. Dimensions must be in a vector with the first element indicating the number of dimensions and the 2:4 elements indicating the x,y,z dimensions.}
  \item{quiet}{If \code{FALSE} information on the cropping process is returned.}
}
\details{
The resizeToDim vector is in this form compatible with the NIFTI dims slot (see \code{\linkS4class{nifti.header}}).
}
\value{
Returns TRUE when succeeded.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{nifti.header}}
}
\keyword{ models }