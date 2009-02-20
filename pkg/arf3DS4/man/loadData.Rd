\name{loadData}
\alias{loadData}

\title{ load data }
\description{
  Load data from a subject/condition from an ARF Experiment 
}
\usage{
loadData(subject,condition,exp=.experiment)
}

\arguments{
  \item{subject}{ Name of the subject }
  \item{condition}{ Name of condition }
  \item{exp}{ ARF experiment object }
  
}
\details{
 If LoadExp was used to load the experiment, the experiment object is passed to LoadData 
}
\value{
 returns the ARF data object
}

\author{ W.D.Weeda }

\keyword{ methods }
