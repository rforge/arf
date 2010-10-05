\name{loadFunc}
\Rdversion{1.1}
\alias{loadFunc}
\title{
Load functional data for a single run of a condition
}
\description{
Load functional data for a single run of a condition
}
\usage{
loadFunc(subject, condition, run, experiment = NULL)
}

\arguments{
  \item{subject}{subject}
  \item{condition}{condition}
  \item{run}{which run (can be numeric, or character)}
  \item{experiment}{.experiment}
}
\value{
Returns an object of class \code{\linkS4class{functional}}.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{functional}}, \code{\linkS4class{experiment}}
}
\keyword{ models }