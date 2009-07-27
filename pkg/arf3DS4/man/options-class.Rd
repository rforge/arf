\name{options-class}
\Rdversion{1.1}
\docType{class}
\alias{options-class}
\alias{.options.adjust.n,options-method}
\alias{.options.adjust.n<-,options-method}
\alias{.options.chk.method,options-method}
\alias{.options.chk.method<-,options-method}
\alias{.options.chk.range,options-method}
\alias{.options.chk.range<-,options-method}
\alias{.options.min.analyticalgrad,options-method}
\alias{.options.min.analyticalgrad<-,options-method}
\alias{.options.min.iterlim,options-method}
\alias{.options.min.iterlim<-,options-method}
\alias{.options.min.routine,options-method}
\alias{.options.min.routine<-,options-method}
\alias{.options.nlm.gradtol,options-method}
\alias{.options.nlm.gradtol<-,options-method}
\alias{.options.nlm.steptol,options-method}
\alias{.options.nlm.steptol<-,options-method}
\alias{.options.opt.lower,options-method}
\alias{.options.opt.lower<-,options-method}
\alias{.options.opt.method,options-method}
\alias{.options.opt.method<-,options-method}
\alias{.options.opt.upper,options-method}
\alias{.options.opt.upper<-,options-method}
\alias{.options.output.mode,options-method}
\alias{.options.output.mode<-,options-method}
\alias{.options.start.maxfac,options-method}
\alias{.options.start.maxfac<-,options-method}
\alias{.options.start.method,options-method}
\alias{.options.start.method<-,options-method}
\alias{.options.start.vector,options-method}
\alias{.options.start.vector<-,options-method}
\alias{.options.sw.type,options-method}
\alias{.options.sw.type<-,options-method}
\alias{.options.version,options-method}
\alias{.options.version<-,options-method}

\title{Class "options" ~~~ }
\description{	 ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("options", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{nlm.gradtol}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{nlm.steptol}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{opt.method}:}{Object of class \code{"character"} ~~ }
    \item{\code{opt.lower}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{opt.upper}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{min.analyticalgrad}:}{Object of class \code{"logical"} ~~ }
    \item{\code{min.iterlim}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{min.routine}:}{Object of class \code{"character"} ~~ }
    \item{\code{start.method}:}{Object of class \code{"character"} ~~ }
    \item{\code{start.maxfac}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{start.vector}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{chk.method}:}{Object of class \code{"character"} ~~ }
    \item{\code{chk.range}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{sw.type}:}{Object of class \code{"character"} ~~ }
    \item{\code{adjust.n}:}{Object of class \code{"logical"} ~~ }
    \item{\code{output.mode}:}{Object of class \code{"character"} ~~ }
    \item{\code{version}:}{Object of class \code{"ANY"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{.options.adjust.n}{\code{signature(object = "options")}: ... }
    \item{.options.adjust.n<-}{\code{signature(x = "options")}: ... }
    \item{.options.chk.method}{\code{signature(object = "options")}: ... }
    \item{.options.chk.method<-}{\code{signature(x = "options")}: ... }
    \item{.options.chk.range}{\code{signature(object = "options")}: ... }
    \item{.options.chk.range<-}{\code{signature(x = "options")}: ... }
    \item{.options.min.analyticalgrad}{\code{signature(object = "options")}: ... }
    \item{.options.min.analyticalgrad<-}{\code{signature(x = "options")}: ... }
    \item{.options.min.iterlim}{\code{signature(object = "options")}: ... }
    \item{.options.min.iterlim<-}{\code{signature(x = "options")}: ... }
    \item{.options.min.routine}{\code{signature(object = "options")}: ... }
    \item{.options.min.routine<-}{\code{signature(x = "options")}: ... }
    \item{.options.nlm.gradtol}{\code{signature(object = "options")}: ... }
    \item{.options.nlm.gradtol<-}{\code{signature(x = "options")}: ... }
    \item{.options.nlm.steptol}{\code{signature(object = "options")}: ... }
    \item{.options.nlm.steptol<-}{\code{signature(x = "options")}: ... }
    \item{.options.opt.lower}{\code{signature(object = "options")}: ... }
    \item{.options.opt.lower<-}{\code{signature(x = "options")}: ... }
    \item{.options.opt.method}{\code{signature(object = "options")}: ... }
    \item{.options.opt.method<-}{\code{signature(x = "options")}: ... }
    \item{.options.opt.upper}{\code{signature(object = "options")}: ... }
    \item{.options.opt.upper<-}{\code{signature(x = "options")}: ... }
    \item{.options.output.mode}{\code{signature(object = "options")}: ... }
    \item{.options.output.mode<-}{\code{signature(x = "options")}: ... }
    \item{.options.start.maxfac}{\code{signature(object = "options")}: ... }
    \item{.options.start.maxfac<-}{\code{signature(x = "options")}: ... }
    \item{.options.start.method}{\code{signature(object = "options")}: ... }
    \item{.options.start.method<-}{\code{signature(x = "options")}: ... }
    \item{.options.start.vector}{\code{signature(object = "options")}: ... }
    \item{.options.start.vector<-}{\code{signature(x = "options")}: ... }
    \item{.options.sw.type}{\code{signature(object = "options")}: ... }
    \item{.options.sw.type<-}{\code{signature(x = "options")}: ... }
    \item{.options.version}{\code{signature(object = "options")}: ... }
    \item{.options.version<-}{\code{signature(x = "options")}: ... }
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
showClass("options")
}
\keyword{classes}
