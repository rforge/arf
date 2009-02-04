\name{newFile}
\alias{newFile}

\title{ Create a new NIFTI file }
\description{
  newFile creates new NIFTI files based on existing header info.
}
\usage{
newFile(filename, templateHDR)
}

\arguments{
  \item{filename}{ What should the name of the file(s) be. }
  \item{templateHDR}{ Path to the NIFTI header file (or .nii file), files may be gzipped. }
}
\value{
 newFile returns an S4 object of class \code{\link{nifti.header-class}}
}

\author{ Wouter D. Weeda }

\keyword{ methods }

