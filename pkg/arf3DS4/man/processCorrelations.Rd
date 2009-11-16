\name{processCorrelations}
\alias{processCorrelations}
\title{Process Interregion correlations.}
\description{Process functional connectivity between regions (inter-region correlations) based on the time-series data.}
\usage{
processCorrelations(tsdata, arfmodel, alpha = 0.05, bonf = T, sort = c("euclid", "correlation", "pvalue", "region"))
}
\arguments{
  \item{tsdata}{Time by region matrix of time-series data.}
  \item{arfmodel}{An arf model object (class \code{\linkS4class{model}})}
  \item{alpha}{Alpha to test the correlations.}
  \item{bonf}{Correct for multiple comparisons?}
  \item{sort}{Sort the output by Euclidian Distance, Highest correlation, Lowest p-value, or Region number.}
}
\value{
Returns a data frame with information on the correlations.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}
}
\keyword{ models }
