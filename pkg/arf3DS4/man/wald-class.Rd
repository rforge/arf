\name{wald-class}
\Rdversion{1.1}
\docType{class}
\alias{wald-class}
\alias{.wald.design,wald-method}
\alias{.wald.design<-,wald-method}
\alias{.wald.df1,wald-method}
\alias{.wald.df1<-,wald-method}
\alias{.wald.df2,wald-method}
\alias{.wald.df2<-,wald-method}
\alias{.wald.pvalues,wald-method}
\alias{.wald.pvalues<-,wald-method}
\alias{.wald.stats,wald-method}
\alias{.wald.stats<-,wald-method}

\title{Class "wald" ~~~ }
\description{	 ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("wald", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{design}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{stats}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{df1}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{df2}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{pvalues}:}{Object of class \code{"matrix"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{.wald.design}{\code{signature(object = "wald")}: ... }
    \item{.wald.design<-}{\code{signature(x = "wald")}: ... }
    \item{.wald.df1}{\code{signature(object = "wald")}: ... }
    \item{.wald.df1<-}{\code{signature(x = "wald")}: ... }
    \item{.wald.df2}{\code{signature(object = "wald")}: ... }
    \item{.wald.df2<-}{\code{signature(x = "wald")}: ... }
    \item{.wald.pvalues}{\code{signature(object = "wald")}: ... }
    \item{.wald.pvalues<-}{\code{signature(x = "wald")}: ... }
    \item{.wald.stats}{\code{signature(object = "wald")}: ... }
    \item{.wald.stats<-}{\code{signature(x = "wald")}: ... }
	 }
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
	or \code{\linkS4class{CLASSNAME}} for links to other classes
}
\examples{
showClass("wald")
}
\keyword{classes}
