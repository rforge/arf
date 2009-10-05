\name{loadModel}
\Rdversion{1.1}
\alias{loadModel}
\title{ load an arf3DS4 model}
\description{
loadModel loads an arf model based on the modelname, subject name and condition name.
}
\usage{
loadModel(modelname, subject = NULL, condition, experiment = NULL)
}
\arguments{
  \item{modelname}{Name of the model to be loaded.}
  \item{subject}{Name of the subject.}
  \item{condition}{Name of the condition.}
  \item{experiment}{Object of class \code{\linkS4Class{experiment}}.}
}
\details{
loadModel can also take as input an object of class \code{\linkS4Class{mnames}} with as a second (\code{subject}) argument an numerical indicator of which model to load.
}
\value{
Returns an object of class \code{\linkS4Class{model}}. 
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\linkS4class{experiment}}, \code{\linkS4class{mnames}}
}
\keyword{ models }
