\name{fitModel}
\alias{fitModel}

\title{ fit an ARF model to data }
\description{
  fitModel fits an ARF model to the data specified in the model object.  
}
\usage{
fitModel(arfmodel, options = loadOptions(arfmodel), dat = readData(.model.avgdatfile(arfmodel)), weights = readData(.model.avgWfile(arfmodel)))
}

\arguments{
  \item{arfmodel}{ an ARF model object }
  \item{options}{ options object, default options are loaded if empty }
  \item{dat}{ fmri data (as a fmri.data object) }
  \item{weights}{ fmri weights (as a fmri.data object) }
}

\value{
 returns an object of class arfmodel
}

\author{ W.D. Weeda }

\keyword{ methods }

