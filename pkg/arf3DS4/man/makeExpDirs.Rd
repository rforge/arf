\name{makeExpDirs}
\alias{makeExpDirs}

\title{ Create an ARF experiment directory structure  }
\description{
  This function creates an experiment directory structure for use with ARF. See reference for details
}
\usage{
makeExpDirs(path = '', name = "default_experiment", subjectind = 1, conditionind = 1, settings = new("settings"))
}

\arguments{
  \item{path}{ name of the full path to the root directory for the experiments. A directory with \code{name} is created if it does not already exist. If path='' the working directory is used as root.}
  \item{name}{ name of the experiment, used to identify different experiment objects. See \link{experiment-class} for details.}
  \item{subjectind}{ a vector of names/identifiers for the subjects. See details. }
  \item{conditionind}{ a vector of names/identifiers for the conditions. See details. }
  \item{settings}{ a settings-class object. When not specified, the default settings are used. See \link{experiment-class} for details.  }
}
\details{
makeExp creates in the directory given by path and name a subjects directory containing directories with names given by each element of \code{subjectind}. 
The experiment directory in the root (given by path) is created using 'recursive=TRUE', which will make all directories given by path. 
In each of the individual subject directories a conditions directory is created. In this directory individual conditions directories are created with names given by each element of \code{conditionind}.
In each individual condition directory a data and model directory are created. In the data directory there are three more subdirectories:
  \item{beta}{ containing the fMRI datafiles, one for each trial.}
  \item{weights}{ containing the fMRI weightfiles, one for each trial.}
  \item{avg}{ containing the ARF averages for the data- and weightfiles.}
  
  
}
\value{
Object of class \code{experiment}.See \link{experiment-class} for details.
}
\references{ Weeda, W.D., Waldorp, L.J., Christoffels, I., and Huizenga, H.M. (in press) Activated Region Fitting: A robust high-power method for fMRI analysis using parameterized regions of activation. Human Brain Mapping, 2009. }
\author{ Wouter D. Weeda }
\note{ 
Note that all names of the directories created can be customized. This is achieved by passing a customized settings object to makeExp. See \link{experiment-class} for details. A settings object is always saved in the root directory of the experiment.
}
\seealso{ \code{\link{experiment-class}} }
\examples{

#create an experiment structure in the current working directory
myexp <- makeExpDirs(path=paste(getwd(),'/myExperiment',sep=''),name='My First Experiment',subj=c('wouter','daniel'),cond=c('A','B','C'))


}
\keyword{ methods }
