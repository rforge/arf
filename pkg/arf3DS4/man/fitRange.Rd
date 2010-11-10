\name{fitRange}
\alias{fitRange}
\title{Fit a range of models}
\description{Fit a range of models to search for an optimal one.}
\usage{
fitRange(subject,condition,range,options=new('options'),modelprefix='searchmodel',modeltype=c('gauss','simple'),experiment=NULL)
}
\arguments{
  \item{subject}{subject}
  \item{condition}{condition}
  \item{range}{range in which to fit}
  \item{options}{An options object}
  \item{modelprefix}{prefix (charachter) to append before modelname}
  \item{modeltype}{Fit a gaussian or simple-gaussian model}
  \item{experiment}{experiment}
}

\value{Returns an object of sequence-class giving the model with the minimum BIC.}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\linkS4class{options}}, \code{\linkS4class{experiment}}
}
\keyword{ models }