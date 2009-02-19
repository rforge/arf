\name{writeData}
\alias{writeData}

\title{ write data in NIFTI/ANALYZE format }
\description{
  writes a vector of fMRI data in a NIFTI/ANALYZE file
}
\usage{
writeData(headinf, datavec)
}

\arguments{
  \item{headinf}{ An object of class 'nifti.header' }
  \item{datavec}{ A numeric vector containing data. }
}
\value{
  returns TRUE if succeeded.
}

\author{ Wouter D. Weeda }

\keyword{methods}
