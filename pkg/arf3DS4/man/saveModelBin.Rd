\name{saveModelBin}
\Rdversion{1.1}
\alias{saveModelBin}
\alias{saveModelBinSimple}
\title{Save an arf model to a NIFTI file.}
\description{
Save the arf model predictions to one or more NIFTI files.
}
\usage{
saveModelBin(arfmodel, type = c("full", "pos", "neg", "fpn", "separate","sig"))
saveModelBinSimple(arfmodel)
}
\arguments{
  \item{arfmodel}{An object of class \code{\linkS4class{model}}.}
  \item{type}{What to save: \code{full}=Full model,\code{pos+neg}=positive + negative file, \code{pos}=Only positive, \code{neg}=Only negative, \code{fpn}=Full, Pos and Neg,\code{separate}=Individual files for each region, \code{sig}=Only significant regions.}
}
\details{
When \code{type} is not 'all, pos or neg' model predictions are not corrected for overlapping regions 
}
\value{
Returns a model object.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}
}
\keyword{ models }
\keyword{ methods }

