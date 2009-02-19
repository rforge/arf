\name{newModel}
\alias{newModel}

\title{ create a new ARF model }
\description{
  make a new model object (and directories) based on an arfdata object (and the current experiment)
}
\usage{
newModel(arfdata, experiment, modelname, options = new("options"), overwrite = T)
}

\arguments{
  \item{arfdata}{ an object of class data}
  \item{experiment}{ an object of class experiment }
  \item{modelname}{ name of the model }
  \item{options}{ an options object (class options) }
  \item{overwrite}{ overwrite models with the same name }
}

\value{
 returns and arfmodel object
}

\author{ W.D. Weeda }

\keyword{ methods }
