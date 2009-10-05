\name{showModels}
\Rdversion{1.1}
\alias{showModels}
\title{List all models for a subject/condition}
\description{
List all models for a subject/condition.
}
\usage{
showModels(subject, condition, experiment = NULL)
}
\arguments{
  \item{subject}{Name of the subject.}
  \item{condition}{Name of the condition.}
  \item{experiment}{Object of class "experiment".}
}
\value{
Returns an object of class \code{\linkS4Class{mnames}} with modelinformation.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{mnames}}, \code{\linkS4class{experiment}}
}
\keyword{ models }
\keyword{ methods }