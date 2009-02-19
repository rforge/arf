\name{readData}
\alias{readData}

\title{ read in NIFTI/ANALYZE data  }
\description{
  readData checks the file to open (using nifti.fileinfo), reads in the header info (using nifti.header), and reads in the data
}
\usage{
readData(filename)
}

\arguments{
  \item{filename}{ Name of the file to open, can be .nii, .hdr, .img }
}

\value{
 readData returns and S4 object of class \code{"\linkS4class{fmri.data}"}
}

\author{ Wouter D. Weeda }

\seealso{ \code{"\linkS4class{fmri.data}"} }


\keyword{ methods }

