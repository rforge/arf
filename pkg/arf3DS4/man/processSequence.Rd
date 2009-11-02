\name{processSequence}
\alias{processSequence}

\title{process a sequence of models}
\description{
processSequence runs a sequence of models with different starting values to obtain a stable solution. It starts with as simple model with rectangular starts, it then runs a full model given the estimates of the simple model. It then prunes the full model, discarding the regions with bad gradients and/or bounded parameters.
}
\usage{
processSequence(modelname = "defaultmodel", seedreg = 10, subject = "", condition = "", options = new("options"), overwrite = T, pr = T, printlevel = 0, try.silen = T, experiment = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modelname}{Name of the model.}
  \item{seedreg}{Number of regions to fit at sequence start.}
  \item{subject}{Name of the subject.}
  \item{condition}{Name of the condition.}
  \item{options}{Options object (default creates a new instance of options, thus loading defaults).}
  \item{overwrite}{Ovewrite the model if a model with the same name alreay exists?}
  \item{pr}{Output to screen?}
  \item{printlevel}{Number indicating the level of output of the minimization procedure.}
  \item{try.silen}{Should errors be printed when they occur or only passed to the \code{warnings} slot.}
  \item{experiment}{Experiment object (as default uses \code{.experiment}).}
}

\value{
Returns the best model (class \code{\linkS4class{model}})
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\link{newModel}}, \code{\link{fitModel}}
}
\keyword{ models }

