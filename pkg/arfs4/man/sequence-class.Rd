\name{sequence-class}
\docType{class}
\alias{sequence-class}


\title{Class "sequence" }
\description{	A sequence class contains information on the models in the sequence (number of regions, fit information, validness) and on which model in the sequence has the best fit.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("sequence", ...)}.
	 
}
\section{Slots}{
	 \describe{
    \item{\code{current}:}{Which model in the sequence is currently processed.}
    \item{\code{regions}:}{Vector of number of regions for each model in the sequence }
    \item{\code{mnames}:}{Vector of names of the models in the sequence (see \code{"\linkS4class{model}"}) }
    \item{\code{fit}:}{Vector of BIC values for each model in the sequence. }
    \item{\code{minimum}:}{Which model in the sequence has the minimal BIC value. }
    \item{\code{valid}:}{Vector indicating which models are valid (i.e. have a converged solution and a valid varcov matrix) }
  }
}
\section{Methods}{
  \describe{
    \item{slots:}{Slots can be accessed via the standard accessor and replacment methods.}

	 }
}

\author{ Wouter D. Weeda }

\keyword{classes}
