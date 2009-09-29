\name{getFileInfo}
\Rdversion{1.1}
\alias{getFileInfo}
\title{Get basic information of a NIFTI/ANALYZE file.}
\description{getFileInfo obtains basic information on fmri data files (NIFTI/ANALYZE). This includes the extension (.nii or .hdr/.img), compression (.gz) and endianness.}
\usage{
getFileInfo(filename)
}

\arguments{
  \item{filename}{Name of the fmri data file.}
}
\value{
Returns an object of class \code{\linkS4class{nifti.fileinfo}}.
}
\references{ See the Nifti homepage (\url{http://nifti.nimh.nih.gov/}) for more information on the nifti filetype.}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{nifti.header}}
}
\keyword{ models }
\keyword{ methods }