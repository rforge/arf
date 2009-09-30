\name{processModel}
\Rdversion{1.1}
\alias{processModel}
\title{Process an arf model.}
\description{Run an entire processing pipeline for a given arf model (minimization and statistics).}
\usage{
processModel(arfmodel, options = loadOptions(arfmodel), dat = readData(.model.avgdatfile(arfmodel)), weights = readData(.model.avgWfile(arfmodel)), printlevel = 0, try.silen = T)
}
\arguments{
  \item{arfmodel}{An arf model object (class \code{\linkS4class{model}})}
  \item{options}{An "options" object (class \code{\linkS4class{options}}). FitModel uses the \code{options@min.routine} slot to determine which minimization procedure to use (either \link{optim} or \link{nlm}. The \code{options@start.method} slot is used to determine which method to use for starting values. The \code{options@modeltype} slot is used to determine whether to fit a full Gauss model or a simple Gauss model.}  
  \item{dat}{An object of class "fmri.data" (\code{\linkS4class{fmri.data}} containing the beta-values.)}
  \item{weights}{An object of class "fmri.data" (\code{\linkS4class{fmri.data}} containing the weights of the beta-values.)}
  \item{printlevel}{Number indicating the level of output of the minimization procedure.}
  \item{try.silen}{Should errors be printed when they occur or only passed to the \code{warnings} slot.}
}
\details{
processModel calls \code{\link{fitModel}}. The model is also automatically saved.
}
\value{
Returns an object of class "model" with the apropriate slots.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\linkS4class{options}}, \code{\linkS4class{fmri.data}}, \code{\link{newModel}}, \code{\link{fitModel}}
}
\keyword{ models }
