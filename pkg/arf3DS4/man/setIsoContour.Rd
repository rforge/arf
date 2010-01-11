\name{setIsoContour}
\alias{setIsoContour}

\title{Create Isocontours}
\description{Create isocontours for each region of the arf model solution.}
\usage{
setIsoContour(arfmodel, conf.int = 95)
}
\arguments{
  \item{arfmodel}{An arf model object (class \code{\linkS4class{model}}).}
  \item{conf.int}{Confidence Interval (in percentage) for the isocontour.}
}
\value{An object of class code{\linkS4class{fmri.data}} with a fourth dimension equal to the number of regions. Each volume contains the voxels within the isocontour for that specific region.}
\\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\linkS4class{fmri.data}}
}
\keyword{ models }
