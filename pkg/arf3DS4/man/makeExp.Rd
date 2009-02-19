\name{makeExp}
\alias{makeExp}

\title{ Make an ARF experiment structure based on existing directories }
\description{
  Based on the path makeExp searches for the appropriate subject and condition directories. 
  Once found the subdirectories of a template subject 'tempsub' and template condition 'tempcond' are check for the appropriate directories. 
  The user is asked to match the found directories to the onse ARF uses. 
}
\usage{
makeExp(path = getwd(), settings = new("settings"), tempsub = 1, tempcond = 1)
}

\arguments{
  \item{path}{ path to the experiment directory. The last dir in the path is taken as the experiment name. }
  \item{settings}{ an object of class 'settings', note that most Dir-values will be overwritten based on the directories found.  }
  \item{tempsub}{ number corresponding to the tempsub-th directory used to check the condition.}
  \item{tempcond}{ number corresponding to the tempcond-th directory used to check the ARF directories. }
}
\details{
	'settings-class' values of subjectDir,conditionDir,avgDir,modelDir,dataDir, and weightsDir are overwritten (but saved in the Experiment.Rda)
}
\value{
returns an object of class \link{experiment-class}

}
\references{ Weeda, W.D., Waldorp, L.J., Christoffels, I., and Huizenga, H.M. (in press) Activated Region Fitting: A robust high-power method for fMRI analysis using parameterized regions of activation. Human Brain Mapping, 2009. }
\author{ Wouter D. Weeda }

\seealso{ \code{\link{experiment-class}} }

\keyword{ methods }

