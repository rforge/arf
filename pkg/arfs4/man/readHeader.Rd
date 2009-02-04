\name{readHeader}
\alias{readHeader}

\title{ read Header information from a NIFTI/ANALYZE file }
\description{
  readHeader reads all header information from a NIFTI/ANALYZE file
}
\usage{
readHeader(fileinf)
}

\arguments{
  \item{fileinf}{ a valid 'nifti.fileinfo' object (see \code{"\linkS4class{nifti.fileinfo}"}) }
}
\details{
  'nifti.fileinfo' objects are only used internally (a call to \link{getFileInfo} calls 'readHeader' and returns header information))
}
\value{
  readHeader returns an S4 object of class 'nifti.header'
}

\author{ Wouter D. Weeda }
 }
\seealso{ \code{"\linkS4class{nifti.fileinfo}"}}


\keyword{ methods }

