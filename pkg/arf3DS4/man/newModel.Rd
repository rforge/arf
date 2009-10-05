\name{newModel}
\Rdversion{1.1}
\alias{newModel}
\title{Create a new arf model}
\description{
newModel creates a new model (including model directories) within a subject/condition, using the options specified.
}
\usage{
newModel(modelname = "defaultmodel", regions = 1, subject = "", condition = "", type = c("gauss", "simple"), options = new("options"), overwrite = T, experiment = NULL)
}
\arguments{
  \item{modelname}{Name of the model.}
  \item{regions}{Number of regions to fit.}
  \item{subject}{Name of the subject.}
  \item{condition}{Name of the condition.}
  \item{type}{Type of model to fit (full Gaussian (\code{gauss}), simple Gaussian (\code{simple}).}
  \item{options}{Options object (default creates a new instance of options, thus loading defaults).}
  \item{overwrite}{Ovewrite the model if a model with the same name alreay exists?}
  \item{experiment}{Experiment object (as default uses \code{.experiment}).}
}
\details{
Calls to newModel also save an instance of a model.Rda in the apropriate directory.
}
\value{
Returns an object of class "model" with the apriopriate slots.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
 \code{\linkS4class{model}}, \code{\linkS4class{options}}, \code{\linkS4class{experiment}}
 }
\keyword{ models }

