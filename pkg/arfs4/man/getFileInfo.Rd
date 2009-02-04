\name{getFileInfo}
\alias{getFileInfo}

\title{ Get information on NIFTI/ANALYZE files}
\description{
 getFileInfo gets information from a given filename and determines the filetype, endianness and if the file is valid.
}
\usage{
getFileInfo(filename)
}

\arguments{
  \item{filename}{ Name of the file to be checked.}
}

\value{
 Returns an S4 object of class \code{\linkS4class{nifti.header}}
}

\author{ Wouter D. Weeda }



\keyword{ methods }

