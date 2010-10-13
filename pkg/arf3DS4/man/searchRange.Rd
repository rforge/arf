\name{searchRange}
\alias{searchRange}
\title{Search range of models for the best fitting.}
\description{Fit a range of models to search for a minimum}
\usage{
searchRange(subject, condition, range = c(10, 50), initial.splits = 4, max.depth = 4, modeltype = c("gauss", "simple"), modelprefix = "searchmodel", options = new("options"), BIC.adjust = T, experiment = NULL)
}
\arguments{
  \item{subject}{subject}
  \item{condition}{condition}
  \item{range}{range in which to fit}
  \item{initial.splits}{how many splits are there initially}
  \item{max.depth}{maximum depth of search}
  \item{modeltype}{Fit a gaussian or simple-gaussian model}
  \item{modelprefix}{prefix (charachter) to append before modelname}
  \item{options}{An options object}
  \item{BIC.adjust}{Adjust the BIC for the simple model to 10 parameters per region.}
  \item{experiment}{experiment}
}

\value{Returns an object of sequence-class giving the model with the minimum BIC.}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\linkS4class{options}}, \code{\linkS4class{experiment}}
}
\keyword{ models }