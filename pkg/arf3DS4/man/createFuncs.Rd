\name{createFuncs}
\Rdversion{1.1}
\alias{createFuncs}
\title{ Create functional directories for trials of an arf condition.
}
\description{
createFuncs creates objects of class \code{\linkS4class{functional}} for each trial in a condition/data directory.
}
\usage{
createFuncs(arfdata)
}
\arguments{
  \item{arfdata}{An object of class "data" (\code{\linkS4class{data}}) from a condition.}
}
\details{
Functional objects are automatically created when an experiment is made by \code{\link{getFSLdata}} when \code{doFuncs = TRUE}. Functional volumes must be linked using \code{\link{setFuncFiles}}. 
}
\value{
None.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{data}},\code{\linkS4class{functional}},\code{\link{getFSLdata}},\code{\link{setFuncFiles}}
}

\keyword{ methods }
