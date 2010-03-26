\name{regressAmplitudes}
\alias{regressAmplitudes}
\title{Estimate amplitude parameters.}

\description{Regress average data to a spatial model of estimates or starting values.}
\usage{
regressAmplitudes(arfmodel, which = "start")
}

\arguments{
  \item{arfmodel}{An \code{\linkS4class{model}} object}
  \item{which}{regress data to startingvalues or estimates}
}

\value{Returns a vector of estimated amplitude for each region.}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}
}
\keyword{ models }
\keyword{ methods }

