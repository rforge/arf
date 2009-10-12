\name{arf2Excel}
\Rdversion{1.1}
\alias{arf2Excel}
\title{ Export model estimates.}
\description{ arf2Excel converts the estimates of an arf model to a tab delimited file that can be opened by excel.}
\usage{
arf2Excel(mod, filename = "estimates.txt")
}

\arguments{
  \item{mod}{An arf model object (see \code{\linkS4class{model}})}
  \item{filename}{Name of the exported file.}
}
\value{
None
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{ \code{\linkS4class{model}}}
\keyword{ models }
