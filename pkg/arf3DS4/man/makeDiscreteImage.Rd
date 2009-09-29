\name{makeDiscreteImage}
\Rdversion{1.1}
\alias{makeDiscreteImage}
\title{ Discretize a vector of fmri data }
\description{
makeDiscreteImage discretizes a vector of fMRI data into a maximum of 64 steps for the purposes of plotting the data. 
}
\usage{
makeDiscreteImage(datavec, zerotol = 0.001)
}
\arguments{
  \item{datavec}{A vector of fMRI data}
  \item{zerotol}{Tolerance at which values of \code{datavec} are taken to be zero.}
}
\value{
Returns a vector of the same length with discretized data.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\keyword{ dplot }
\keyword{ models }