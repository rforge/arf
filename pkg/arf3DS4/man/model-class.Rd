\name{model-class}
\Rdversion{1.1}
\docType{class}
\alias{model-class}
\alias{.model.avgdatfile,model-method}
\alias{.model.avgdatfile<-,model-method}
\alias{.model.avgWfile,model-method}
\alias{.model.avgWfile<-,model-method}
\alias{.model.betafiles,model-method}
\alias{.model.betafiles<-,model-method}
\alias{.model.convergence,model-method}
\alias{.model.convergence<-,model-method}
\alias{.model.derivativeFile,model-method}
\alias{.model.derivativeFile<-,model-method}
\alias{.model.estimates,model-method}
\alias{.model.estimates<-,model-method}
\alias{.model.fit,model-method}
\alias{.model.fit<-,model-method}
\alias{.model.fullmodelDataFile,model-method}
\alias{.model.fullmodelDataFile<-,model-method}
\alias{.model.fullpath,model-method}
\alias{.model.fullpath<-,model-method}
\alias{.model.funcDir,model-method}
\alias{.model.funcDir<-,model-method}
\alias{.model.funcRda,model-method}
\alias{.model.funcRda<-,model-method}
\alias{.model.hessian,model-method}
\alias{.model.hessian<-,model-method}
\alias{.model.iterates,model-method}
\alias{.model.iterates<-,model-method}
\alias{.model.minimum,model-method}
\alias{.model.minimum<-,model-method}
\alias{.model.modelDataFile,model-method}
\alias{.model.modelDataFile<-,model-method}
\alias{.model.modeldatapath,model-method}
\alias{.model.modeldatapath<-,model-method}
\alias{.model.modelFile,model-method}
\alias{.model.modelFile<-,model-method}
\alias{.model.modelname,model-method}
\alias{.model.modelname<-,model-method}
\alias{.model.modelpath,model-method}
\alias{.model.modelpath<-,model-method}
\alias{.model.name,model-method}
\alias{.model.name<-,model-method}
\alias{.model.optionsFile,model-method}
\alias{.model.optionsFile<-,model-method}
\alias{.model.proctime,model-method}
\alias{.model.proctime<-,model-method}
\alias{.model.regDir,model-method}
\alias{.model.regDir<-,model-method}
\alias{.model.regions,model-method}
\alias{.model.regions<-,model-method}
\alias{.model.regRda,model-method}
\alias{.model.regRda<-,model-method}
\alias{.model.residualFile,model-method}
\alias{.model.residualFile<-,model-method}
\alias{.model.sandwichmethod,model-method}
\alias{.model.sandwichmethod<-,model-method}
\alias{.model.startFile,model-method}
\alias{.model.startFile<-,model-method}
\alias{.model.startval,model-method}
\alias{.model.startval<-,model-method}
\alias{.model.trials,model-method}
\alias{.model.trials<-,model-method}
\alias{.model.valid,model-method}
\alias{.model.valid<-,model-method}
\alias{.model.varcov,model-method}
\alias{.model.varcov<-,model-method}
\alias{.model.version,model-method}
\alias{.model.version<-,model-method}
\alias{.model.wald,model-method}
\alias{.model.wald<-,model-method}
\alias{.model.warnings,model-method}
\alias{.model.warnings<-,model-method}
\alias{.model.weightFile,model-method}
\alias{.model.weightFile<-,model-method}
\alias{.model.weightfiles,model-method}
\alias{.model.weightfiles<-,model-method}
\alias{show,model-method}

\title{Class "model" ~~~ }
\description{	 ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("model", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{modelname}:}{Object of class \code{"character"} ~~ }
    \item{\code{modelpath}:}{Object of class \code{"character"} ~~ }
    \item{\code{modeldatapath}:}{Object of class \code{"character"} ~~ }
    \item{\code{residualFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{derivativeFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{weightFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{modelDataFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{fullmodelDataFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{modelFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{optionsFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{startFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{convergence}:}{Object of class \code{"character"} ~~ }
    \item{\code{iterates}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{minimum}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{estimates}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{hessian}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{sandwichmethod}:}{Object of class \code{"character"} ~~ }
    \item{\code{varcov}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{warnings}:}{Object of class \code{"character"} ~~ }
    \item{\code{fit}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{wald}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{regions}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{startval}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{proctime}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{valid}:}{Object of class \code{"logical"} ~~ }
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{fullpath}:}{Object of class \code{"character"} ~~ }
    \item{\code{betafiles}:}{Object of class \code{"character"} ~~ }
    \item{\code{weightfiles}:}{Object of class \code{"character"} ~~ }
    \item{\code{avgdatfile}:}{Object of class \code{"character"} ~~ }
    \item{\code{avgWfile}:}{Object of class \code{"character"} ~~ }
    \item{\code{regDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{regRda}:}{Object of class \code{"character"} ~~ }
    \item{\code{funcDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{funcRda}:}{Object of class \code{"character"} ~~ }
    \item{\code{trials}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{version}:}{Object of class \code{"ANY"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{data}"}, directly.
}
\section{Methods}{
  \describe{
    \item{.model.avgdatfile}{\code{signature(object = "model")}: ... }
    \item{.model.avgdatfile<-}{\code{signature(x = "model")}: ... }
    \item{.model.avgWfile}{\code{signature(object = "model")}: ... }
    \item{.model.avgWfile<-}{\code{signature(x = "model")}: ... }
    \item{.model.betafiles}{\code{signature(object = "model")}: ... }
    \item{.model.betafiles<-}{\code{signature(x = "model")}: ... }
    \item{.model.convergence}{\code{signature(object = "model")}: ... }
    \item{.model.convergence<-}{\code{signature(x = "model")}: ... }
    \item{.model.derivativeFile}{\code{signature(object = "model")}: ... }
    \item{.model.derivativeFile<-}{\code{signature(x = "model")}: ... }
    \item{.model.estimates}{\code{signature(object = "model")}: ... }
    \item{.model.estimates<-}{\code{signature(x = "model")}: ... }
    \item{.model.fit}{\code{signature(object = "model")}: ... }
    \item{.model.fit<-}{\code{signature(x = "model")}: ... }
    \item{.model.fullmodelDataFile}{\code{signature(object = "model")}: ... }
    \item{.model.fullmodelDataFile<-}{\code{signature(x = "model")}: ... }
    \item{.model.fullpath}{\code{signature(object = "model")}: ... }
    \item{.model.fullpath<-}{\code{signature(x = "model")}: ... }
    \item{.model.funcDir}{\code{signature(object = "model")}: ... }
    \item{.model.funcDir<-}{\code{signature(x = "model")}: ... }
    \item{.model.funcRda}{\code{signature(object = "model")}: ... }
    \item{.model.funcRda<-}{\code{signature(x = "model")}: ... }
    \item{.model.hessian}{\code{signature(object = "model")}: ... }
    \item{.model.hessian<-}{\code{signature(x = "model")}: ... }
    \item{.model.iterates}{\code{signature(object = "model")}: ... }
    \item{.model.iterates<-}{\code{signature(x = "model")}: ... }
    \item{.model.minimum}{\code{signature(object = "model")}: ... }
    \item{.model.minimum<-}{\code{signature(x = "model")}: ... }
    \item{.model.modelDataFile}{\code{signature(object = "model")}: ... }
    \item{.model.modelDataFile<-}{\code{signature(x = "model")}: ... }
    \item{.model.modeldatapath}{\code{signature(object = "model")}: ... }
    \item{.model.modeldatapath<-}{\code{signature(x = "model")}: ... }
    \item{.model.modelFile}{\code{signature(object = "model")}: ... }
    \item{.model.modelFile<-}{\code{signature(x = "model")}: ... }
    \item{.model.modelname}{\code{signature(object = "model")}: ... }
    \item{.model.modelname<-}{\code{signature(x = "model")}: ... }
    \item{.model.modelpath}{\code{signature(object = "model")}: ... }
    \item{.model.modelpath<-}{\code{signature(x = "model")}: ... }
    \item{.model.name}{\code{signature(object = "model")}: ... }
    \item{.model.name<-}{\code{signature(x = "model")}: ... }
    \item{.model.optionsFile}{\code{signature(object = "model")}: ... }
    \item{.model.optionsFile<-}{\code{signature(x = "model")}: ... }
    \item{.model.proctime}{\code{signature(object = "model")}: ... }
    \item{.model.proctime<-}{\code{signature(x = "model")}: ... }
    \item{.model.regDir}{\code{signature(object = "model")}: ... }
    \item{.model.regDir<-}{\code{signature(x = "model")}: ... }
    \item{.model.regions}{\code{signature(object = "model")}: ... }
    \item{.model.regions<-}{\code{signature(x = "model")}: ... }
    \item{.model.regRda}{\code{signature(object = "model")}: ... }
    \item{.model.regRda<-}{\code{signature(x = "model")}: ... }
    \item{.model.residualFile}{\code{signature(object = "model")}: ... }
    \item{.model.residualFile<-}{\code{signature(x = "model")}: ... }
    \item{.model.sandwichmethod}{\code{signature(object = "model")}: ... }
    \item{.model.sandwichmethod<-}{\code{signature(x = "model")}: ... }
    \item{.model.startFile}{\code{signature(object = "model")}: ... }
    \item{.model.startFile<-}{\code{signature(x = "model")}: ... }
    \item{.model.startval}{\code{signature(object = "model")}: ... }
    \item{.model.startval<-}{\code{signature(x = "model")}: ... }
    \item{.model.trials}{\code{signature(object = "model")}: ... }
    \item{.model.trials<-}{\code{signature(x = "model")}: ... }
    \item{.model.valid}{\code{signature(object = "model")}: ... }
    \item{.model.valid<-}{\code{signature(x = "model")}: ... }
    \item{.model.varcov}{\code{signature(object = "model")}: ... }
    \item{.model.varcov<-}{\code{signature(x = "model")}: ... }
    \item{.model.version}{\code{signature(object = "model")}: ... }
    \item{.model.version<-}{\code{signature(x = "model")}: ... }
    \item{.model.wald}{\code{signature(object = "model")}: ... }
    \item{.model.wald<-}{\code{signature(x = "model")}: ... }
    \item{.model.warnings}{\code{signature(object = "model")}: ... }
    \item{.model.warnings<-}{\code{signature(x = "model")}: ... }
    \item{.model.weightFile}{\code{signature(object = "model")}: ... }
    \item{.model.weightFile<-}{\code{signature(x = "model")}: ... }
    \item{.model.weightfiles}{\code{signature(object = "model")}: ... }
    \item{.model.weightfiles<-}{\code{signature(x = "model")}: ... }
    \item{show}{\code{signature(object = "model")}: ... }
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
showClass("model")
}
\keyword{classes}
