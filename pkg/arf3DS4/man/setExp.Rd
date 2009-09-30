\name{setExp}
\Rdversion{1.1}
\alias{setExp}
\title{Set values of an experiment}
\description{
Set values of an experiment using the data in the directory structure.
}
\usage{
setExp(path = getwd(), tempsub = 1, tempcond = 1, auto = TRUE, createWeights = TRUE, overwrite = F, load = T)
}
\arguments{
  \item{path}{Path to the experiment.}
  \item{tempsub}{Template subject, used to obtain some values.}
  \item{tempcond}{Template condition, used to obtain some values.}
  \item{auto}{Automatically detect directories or ask?}
  \item{createWeights}{Create weights for beta-values.}
  \item{overwrite}{Overwrite old-values.}
  \item{load}{Load experiment after setting?}
}
\details{
setExp is called by \code{\link{loadExp}} when option is \code{'set'}
}
\value{
Returns an object of class \code{\linkS4Class{experiment}}.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{experiment}}, \code{\link{loadExp}}
}
\keyword{ models }
