\name{pruneModel}
\alias{pruneModel}
\title{Prune a model based on invalid gradients and bounded parameters.}
\description{Prune model prunes an arfmodel according to the gradients and bounds to obtain a valid solution.}
\usage{
pruneModel(arfmodel,modelname='defaultmodel',subject='',condition='',grad=NULL,bound=NULL,pval=NULL,options=new('options'),overwrite=T,experiment=NULL)
}

\arguments{
  \item{arfmodel}{An arf model object (class \code{\linkS4class{model}})}
  \item{modelname}{Name for the new models (pruneModel makes newmodelnames).}
  \item{subject}{Name of the subject.}
  \item{condition}{Name of the condition.}
  \item{grad}{Absolute threshold for gradient check.}
  \item{bound}{Number of digits used in equality check on the bound.}
  \item{pval}{Prune non-significant regions?}
  \item{options}{An "options" object (class \code{\linkS4class{options}}).} 
  \item{overwrite}{Overwrite existing objects.}
  \item{experiment}{Experiment object (as default uses \code{.experiment}).}
}
\value{
Returns an object of class "model" with the apropriate slots.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\linkS4class{options}}, \code{\linkS4class{experiment}}
}
\keyword{ models }
