\name{saveModelBin}
\Rdversion{1.1}
\alias{saveModelBin}
\alias{saveModelBinSimple}
\title{Save ARF model estimates to a NIFTI file.}
\description{
Save ARF model estimates to a NIFTI file. It has several options regarding which regions of the spatial model are saved.
}
\usage{
saveModelBin(arfmodel, type = c("full", "pos", "neg", "fpn", "separate","sig"))
saveModelBinSimple(arfmodel)
}
\arguments{
  \item{arfmodel}{An object of class \code{\linkS4class{model}}.}
  \item{type}{What to save: \code{'full'}=Full model,\code{'pos+neg'}=positive + negative regions, \code{'pos'}=Only positive regions, \code{'neg'}=Only negative regions, \code{'fpn'}=Full, positive and negative regions (3 files), \code{'separate'}=Individual files for each region, \code{'sig'}=Only significant regions.}
}
\details{
When \code{type} is not \code{'all'}, \code{'pos'} or \code{'neg'} model predictions are not corrected for overlapping regions. 
}
\value{
Returns a model object.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\link{writeData}}, \code{\linkS4class{fmri.data}}
}
\keyword{ models }
\keyword{ methods }

