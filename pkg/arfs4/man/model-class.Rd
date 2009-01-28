\name{model-class}
\docType{class}
\alias{model-class}

\title{Class "model" ~~~ }
\description{The 'model' class contains all model information (plus information from the data class slots), convergence, fit, estimates, variances, statistics, etc. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("model", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{modelname}:}{Name of the model, defaults to the number of regions. }
    \item{\code{convergence}:}{Information on convergence (converged and why, or not converged and why). }
    \item{\code{minimum}:}{Value of the objective funtion at the minimum.}
    \item{\code{estimates}:}{Parameter estimates.}
    \item{\code{hessian}:}{Hessian matrix as returned by nlm. }
    \item{\code{varcov}:}{Variance/covariance matrix. }
    \item{\code{warnings}:}{Additional warings (if varcov is singular or not fullrank).}
    \item{\code{fit}:}{Bayesian Information Criterion (BIC) value of the model. }
    \item{\code{wald}:}{S4 object containing Wald statistics, see \code{\linkS4class{wald}}}
    \item{\code{regions}:}{Number of regions fitted. }
    \item{\code{startval}:}{Vector of starting values.}
    \item{\code{proctime}:}{Time for nlm to converge (or not).}
    \item{\code{valid}:}{Is the model valid? }
  	\item{\code{name}:}{Name of the condition. (\code{"character"}) }
    \item{\code{fullpath}:}{Path to the rootdirectory of the condition (\code{"character"}) }
    \item{\code{datafiles}:}{Character vector of paths to the datafiles (in the 'data' directory') }
    \item{\code{weightfiles}:}{Character vector of paths to the weightfiles (in the 'weights' directory')  }
    \item{\code{avgdatfile}:}{Path to the averaged data (\code{"character"}) }
    \item{\code{avgWfile}:}{Path to the averaged weights (\code{"character"}) }
    \item{\code{trials}:}{Integer indicating the number of trials }
  }
}
\section{Extends}{
Class \code{"\linkS4class{data}"}, directly.
}

\section{Methods}{
  \describe{
    \item{slots:}{Slots can be accessed via the standard accessor and replacment methods.}

	 }
}

\author{ Wouter D. Weeda }
 
\examples{

}

\keyword{classes}


