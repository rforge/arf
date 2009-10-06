\name{mcpCorrect}
\Rdversion{1.1}
\alias{mcpCorrect}
\title{ Calculate Multiple Comparison Corrections }
\description{
mcpCorrect calculates three types of multiple comparison corrections: 'uncorrected', 'bonferroni', and 'False Discovery Rate (FDR)'. mcpCorrect assumes the data are t-values.
}
\usage{
mcpCorrect(fmridata, type = c("uncorrected", "bonferroni", "FDR"), alpha = 0.05, q = 0.05, cv = 1, df = 100, sig.steps = 1, adj.n = T)
}
\arguments{
  \item{fmridata}{An object of class ".fmri.data" (see \code{\linkS4Class{fmri.data}}).}
  \item{type}{Type of correction (\code{uncorrected},\code{bonferroni},\code{FDR})}
  \item{alpha}{Nominal alpha level.}
  \item{q}{q parameter for FDR.}
  \item{cv}{Cv parameter for FDR.}
  \item{df}{Degrees of freedom of the t values.}
  \item{sig.steps}{Number of steps to divide p-values in.}
  \item{adj.n}{Use only brain voxels when correcting?}
}
\value{
Returns an object of class "fmri.data" with superthreshold voxels.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{fmri.data}}
}
\keyword{ models }

