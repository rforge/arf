\name{analysis-class}
\docType{class}
\alias{analysis-class}


\title{Class "analysis" ~~~ }
\description{The analysis class contains the designmatrix to test hypotheses between conditions and/or subjects using the objects defined in the modelobjects slot.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("analysis", ...)}.

}
\section{Slots}{
	 \describe{
    \item{\code{name}:}{Name of the analysis (\code{"character"})}
    \item{\code{modelobjects}:}{Objects to be used in the analysis.}
    \item{\code{designmatrix}:}{Matrix containing the hypotheses to be tested.}
    \item{\code{rfx}:}{Whether or not to perform Random Effects analysis}
  }
}
\section{Methods}{
  \describe{
    \item{slots:}{Slots can be accessed via the standard accessor and replacment methods.}

	 }
}

\author{ Wouter D. Weeda }
\note{ THIS CLASS IS NOT YET USED IN THE ARF PROCEDURE!}

 
\examples{

}

\keyword{classes}
