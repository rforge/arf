\name{arfcorrelation-class}
\Rdversion{1.1}
\docType{class}
\alias{arfcorrelation-class}
\alias{.arfcorrelation.correlation,arfcorrelation-method}
\alias{.arfcorrelation.correlation.pval,arfcorrelation-method}
\alias{.arfcorrelation.correlation.pval<-,arfcorrelation-method}
\alias{.arfcorrelation.correlation<-,arfcorrelation-method}
\alias{.arfcorrelation.number.correlations,arfcorrelation-method}
\alias{.arfcorrelation.number.correlations<-,arfcorrelation-method}
\alias{.arfcorrelation.partial.correlation,arfcorrelation-method}
\alias{.arfcorrelation.partial.correlation.pval,arfcorrelation-method}
\alias{.arfcorrelation.partial.correlation.pval<-,arfcorrelation-method}
\alias{.arfcorrelation.partial.correlation<-,arfcorrelation-method}
\alias{.arfcorrelation.timebyregion,arfcorrelation-method}
\alias{.arfcorrelation.timebyregion<-,arfcorrelation-method}

\title{Class "arfcorrelation"}
\description{ ARF Correlation class holds correlations between regions of the model over trials.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("arfcorrelation", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{timebyregion}:}{Trial-by-trial amplitude estimates per region (time x nreg)}
    \item{\code{correlation}:}{Correlation matrix.}
    \item{\code{correlation.pval}:}{P-values of the correlations}
    \item{\code{partial.correlation}:}{Partial correlation matrix (corrected for all other regions).}
    \item{\code{partial.correlation.pval}:}{P-values of partial correlations}
    \item{\code{number.correlations}:}{Number of correlations testes}
  }
}

\section{Methods}{
Standard arf3DS4 accessor and replacement functions can be used to access and replace slots.  
  \describe{
    \item{access}{\code{.classname.slotname(object)}}
    \item{replace}{\code{.classname.slotname(object)<-}}
	 }
In this case classname is "arfcorrelation" and slotname is for example "correlation". So to access the "correlation" slot of an r-object "arfcor"  (of class "arfcorrelation") type \code{.arfcorrelation.correlation(arfcor)}.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\examples{
showClass("arfcorrelation")
}
\keyword{classes}

