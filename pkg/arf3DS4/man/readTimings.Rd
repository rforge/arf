\name{readTimings}
\Rdversion{1.1}
\alias{readTimings}
\title{Read timings from a .bfsl file.}
\description{Read timings from a .bfsl file. Also read in stimulus lengths.}
\usage{
readTimings(bfslfile)
}

\arguments{
  \item{bfslfile}{Filename of the .bfsl file}
}
\value{
Returns a vector with timings (in seconds) with a \code{stimlen} attribute.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }

\keyword{ methods }
\keyword{ models }
