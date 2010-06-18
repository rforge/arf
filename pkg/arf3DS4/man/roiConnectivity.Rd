\name{roiConnectivity}
\alias{roiConnectivity}

\title{calculate Connectivity with spatial averaging}
\description{Calculates functional connectivity bases on the spatial average of the voxels with in the isoContours of the estimated model}
\usage{
roiConnectivity(arfmodel,roidata=setIsoContour(arfmodel,95),funcfilename='single_events.nii.gz',type=c('avg','ev'),evmodel=c('spatial','spatiotemporal','eigenvariate'))
}

\arguments{
  \item{arfmodel}{An arf model object (class \code{\linkS4class{model}})}
  \item{roidata}{fmri.data object containing ROIs for each region (one per volume).}
  \item{funcfilename}{Filename of the volume to extract the raw time-series.}
  \item{type}{Average using no weighing (avg) or by weighing with the first spatial eigenvector.}
  \item{evmodel}{Wich data tot use to calclate the first spatial eigenvector. Choose: Modelestimates, averaged data, roidata. }
  
}

\value{
Returns an object of class "arfcorrelation" including the region by time matrix of amplitude time-series and the estimated correlations.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}
}
\keyword{ models }