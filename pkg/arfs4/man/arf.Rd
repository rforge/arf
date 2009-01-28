\name{arf}
\alias{arf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Activated Region Fitting }
\description{
  A call to \code{arf} starts the ARF fitting procedure. It checks if the given \code{path} contains a "data" and a "weights" directory and creates the necessary S4 classes and average files
  (if enabled). The name of the experiment can be entered via \code{expname}, this is only used for output on screen. The \code{sequence} and \code{regionstofit} parameters
  define the number of regions that are fitted and in which order (see the arguments section for details). The \code{fast} option enables/disables calculation of sandwich estimates at each
  model that is fitted or only at the best fitting model. \code{fullOutput} controls the amount of output shown on screen. \code{createAverages} controls the creation of the average files.
  
}
\usage{
arf(path = "", expname = NULL, sequence = c("manual", "sequential", "interleaved"), regionstofit = 1, fast = F, fullOutput = T, createAverages = T)
}

\arguments{
  \item{path}{ The directory containing the data (it assumes the "data" and "weights" directories exist in the given path). See \code{\link{makeDirStruct}} on how to automatically create a proper directory structure. }
  \item{expname}{ The name of the experiment/condition }
  \item{sequence}{ Can be either \code{manual}, \code{sequential}, or \code{interleaved}.}
  \item{regionstofit}{ With sequence set to 'manual': a vector containing the sequence of models to fit (values indicate the number of regions to fit). With sequence set to 'sequential': an integer giving the maximum number of regions to fit. ARF then fits all models from 1 to the maximum. With sequence set to 'interleaved': an integer giving the maximum number of regions to fit. ARF then fits all odd models from 1 to the maximum.  }
  \item{fast}{ When fast is \code{FALSE} (default) the sandwich estimates are calculated at each model in the sequence. When fast is \code{TRUE} sandwich estimates are only calculated for the best fitting model. }
  \item{fullOutput}{ When fullOutput is \code{TRUE} (default) all output is displayed, when \code{FALSE} only selected output is given.}
  \item{createAverages}{ When createAverages is \code{TRUE} (default) averages are calculated, when \code{FALSE} they are not (and are assumed to exist in the 'avg' directory.}
}
\details{
  ARF can read in NIFTI formatted files or analyze files, assuming a neurological orientation. The NIFTI/ANALYZE header parameters (file type etc.) are taken from the first file in the 'data' directory. For read/write related functions see \code{\link{getFileInfo}},c ode{\link{newFile}}, code{\link{readData}}, code{\link{readHeader}}, code{\link{writeData}}, code{\link{writeHeader}}.
}
\value{
none. The S4 object of class \code{data} of each model can be accessed via the .RDA files in the 'stats' directory. The S4 object of class \code{sequence} containing the model fits can also be accessed via this directory. See code{\linkS4class{data}} and code{\linkS4class{sequence}} for details on the S4 classes.
}
\references{ Weeda, W.D., Waldorp, L.J., Christoffels, I., and Huizenga, H.M. (in press) Activated Region Fitting: A robust high-power method for fMRI analysis using parameterized regions of activation. Human Brain Mapping, 2009. }
\author{ Wouter D.Weeda }

\examples{

 

}	
	
\keyword{methods}

