\name{experiment-class}
\Rdversion{1.1}
\docType{class}
\alias{experiment-class}
\alias{.experiment.avgdatFile,experiment-method}
\alias{.experiment.avgdatFile<-,experiment-method}
\alias{.experiment.avgDir,experiment-method}
\alias{.experiment.avgDir<-,experiment-method}
\alias{.experiment.avgtstatFile,experiment-method}
\alias{.experiment.avgtstatFile<-,experiment-method}
\alias{.experiment.avgWFile,experiment-method}
\alias{.experiment.avgWFile<-,experiment-method}
\alias{.experiment.betaDir,experiment-method}
\alias{.experiment.betaDir<-,experiment-method}
\alias{.experiment.condition.names,experiment-method}
\alias{.experiment.condition.names<-,experiment-method}
\alias{.experiment.condition.num,experiment-method}
\alias{.experiment.condition.num<-,experiment-method}
\alias{.experiment.conditionDir,experiment-method}
\alias{.experiment.conditionDir<-,experiment-method}
\alias{.experiment.conditionPrefix,experiment-method}
\alias{.experiment.conditionPrefix<-,experiment-method}
\alias{.experiment.dataDir,experiment-method}
\alias{.experiment.dataDir<-,experiment-method}
\alias{.experiment.dataRda,experiment-method}
\alias{.experiment.dataRda<-,experiment-method}
\alias{.experiment.derivativeFile,experiment-method}
\alias{.experiment.derivativeFile<-,experiment-method}
\alias{.experiment.expRda,experiment-method}
\alias{.experiment.expRda<-,experiment-method}
\alias{.experiment.funcDir,experiment-method}
\alias{.experiment.funcDir<-,experiment-method}
\alias{.experiment.funcRda,experiment-method}
\alias{.experiment.funcRda<-,experiment-method}
\alias{.experiment.modelDataFile,experiment-method}
\alias{.experiment.modelDataFile<-,experiment-method}
\alias{.experiment.modeldatDir,experiment-method}
\alias{.experiment.modeldatDir<-,experiment-method}
\alias{.experiment.modelDir,experiment-method}
\alias{.experiment.modelDir<-,experiment-method}
\alias{.experiment.modelnamesRda,experiment-method}
\alias{.experiment.modelnamesRda<-,experiment-method}
\alias{.experiment.modelPrefix,experiment-method}
\alias{.experiment.modelPrefix<-,experiment-method}
\alias{.experiment.modelRda,experiment-method}
\alias{.experiment.modelRda<-,experiment-method}
\alias{.experiment.name,experiment-method}
\alias{.experiment.name<-,experiment-method}
\alias{.experiment.optionsRda,experiment-method}
\alias{.experiment.optionsRda<-,experiment-method}
\alias{.experiment.path,experiment-method}
\alias{.experiment.path<-,experiment-method}
\alias{.experiment.regDir,experiment-method}
\alias{.experiment.regDir<-,experiment-method}
\alias{.experiment.regRda,experiment-method}
\alias{.experiment.regRda<-,experiment-method}
\alias{.experiment.residualFile,experiment-method}
\alias{.experiment.residualFile<-,experiment-method}
\alias{.experiment.startRda,experiment-method}
\alias{.experiment.startRda<-,experiment-method}
\alias{.experiment.statsDir,experiment-method}
\alias{.experiment.statsDir<-,experiment-method}
\alias{.experiment.statsRda,experiment-method}
\alias{.experiment.statsRda<-,experiment-method}
\alias{.experiment.subject.names,experiment-method}
\alias{.experiment.subject.names<-,experiment-method}
\alias{.experiment.subject.num,experiment-method}
\alias{.experiment.subject.num<-,experiment-method}
\alias{.experiment.subjectDir,experiment-method}
\alias{.experiment.subjectDir<-,experiment-method}
\alias{.experiment.subjectPrefix,experiment-method}
\alias{.experiment.subjectPrefix<-,experiment-method}
\alias{.experiment.version,experiment-method}
\alias{.experiment.version<-,experiment-method}
\alias{.experiment.weightFile,experiment-method}
\alias{.experiment.weightFile<-,experiment-method}
\alias{.experiment.weightsDir,experiment-method}
\alias{.experiment.weightsDir<-,experiment-method}
\alias{show,experiment-method}

\title{Class "experiment" ~~~ }
\description{	 ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("experiment", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{path}:}{Object of class \code{"character"} ~~ }
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{subject.num}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{subject.names}:}{Object of class \code{"character"} ~~ }
    \item{\code{condition.num}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{condition.names}:}{Object of class \code{"character"} ~~ }
    \item{\code{expRda}:}{Object of class \code{"character"} ~~ }
    \item{\code{optionsRda}:}{Object of class \code{"character"} ~~ }
    \item{\code{startRda}:}{Object of class \code{"character"} ~~ }
    \item{\code{dataRda}:}{Object of class \code{"character"} ~~ }
    \item{\code{modelRda}:}{Object of class \code{"character"} ~~ }
    \item{\code{statsRda}:}{Object of class \code{"character"} ~~ }
    \item{\code{regRda}:}{Object of class \code{"character"} ~~ }
    \item{\code{funcRda}:}{Object of class \code{"character"} ~~ }
    \item{\code{subjectPrefix}:}{Object of class \code{"character"} ~~ }
    \item{\code{conditionPrefix}:}{Object of class \code{"character"} ~~ }
    \item{\code{modelPrefix}:}{Object of class \code{"character"} ~~ }
    \item{\code{subjectDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{conditionDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{dataDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{weightsDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{avgDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{regDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{funcDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{betaDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{modelDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{statsDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{modeldatDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{avgdatFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{avgWFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{avgtstatFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{modelDataFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{modelnamesRda}:}{Object of class \code{"character"} ~~ }
    \item{\code{residualFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{derivativeFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{weightFile}:}{Object of class \code{"character"} ~~ }
    \item{\code{version}:}{Object of class \code{"ANY"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{settings}"}, directly.
}
\section{Methods}{
  \describe{
    \item{.experiment.avgdatFile}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.avgdatFile<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.avgDir}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.avgDir<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.avgtstatFile}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.avgtstatFile<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.avgWFile}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.avgWFile<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.betaDir}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.betaDir<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.condition.names}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.condition.names<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.condition.num}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.condition.num<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.conditionDir}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.conditionDir<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.conditionPrefix}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.conditionPrefix<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.dataDir}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.dataDir<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.dataRda}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.dataRda<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.derivativeFile}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.derivativeFile<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.expRda}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.expRda<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.funcDir}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.funcDir<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.funcRda}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.funcRda<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.modelDataFile}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.modelDataFile<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.modeldatDir}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.modeldatDir<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.modelDir}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.modelDir<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.modelnamesRda}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.modelnamesRda<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.modelPrefix}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.modelPrefix<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.modelRda}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.modelRda<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.name}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.name<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.optionsRda}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.optionsRda<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.path}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.path<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.regDir}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.regDir<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.regRda}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.regRda<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.residualFile}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.residualFile<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.startRda}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.startRda<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.statsDir}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.statsDir<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.statsRda}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.statsRda<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.subject.names}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.subject.names<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.subject.num}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.subject.num<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.subjectDir}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.subjectDir<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.subjectPrefix}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.subjectPrefix<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.version}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.version<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.weightFile}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.weightFile<-}{\code{signature(x = "experiment")}: ... }
    \item{.experiment.weightsDir}{\code{signature(object = "experiment")}: ... }
    \item{.experiment.weightsDir<-}{\code{signature(x = "experiment")}: ... }
    \item{show}{\code{signature(object = "experiment")}: ... }
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
showClass("experiment")
}
\keyword{classes}
