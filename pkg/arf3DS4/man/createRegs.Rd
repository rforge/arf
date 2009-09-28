\name{createRegs}
\Rdversion{1.1}
\alias{createRegs}
\title{ Create registration directories for trials of an arf condition.
}
\description{
createRegs creates registration directories for trials of an arf condition and sets the appropriate paths.
}
\usage{
createRegs(arfdata)
}
\arguments{
  \item{arfdata}{An object of class "data" (\code{\linkS4class{data}}) from a condition.}
}
\details{
Registration objects are automatically created when an experiment is made by \code{\link{getFSLdata}} when \code{doRegs = TRUE}. Registration matrices and volumes must be linked using \code{\link{setRegFiles}} and \code{\link{setRegParams}}.
}
\value{
None
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{data}},\code{\linkS4class{registration}},\code{\link{getFSLdata}},\code{\link{setRegFiles}},\code{\link{setRegParams}}
}

\keyword{ methods }
