\name{makeDirStruct}
\alias{makeDirStruct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a directory structure for ARF analysis }
\description{
  makeDirstruct makes a full directory structure needed for arf analysis.
}
\usage{
makeDirStruct(rootdir = "", studyname = "undefined_experiment", numsubs = 1, subdirname = "subject", condpersub = 1, conddirname = "condition", showstructure = T)
}

\arguments{
  \item{rootdir}{ Rootdirectory of the experiment. }
  \item{studyname}{ Name of the the experiment. }
  \item{numsubs}{ Number of subjects in the experiment. }
  \item{subdirname}{ Name of the subject directories (appends subject number), default is 'subject' }
  \item{condpersub}{ Number of conditions per subject. }
  \item{conddirname}{ Name of the condition directories (appends condition number), default is 'condition' }
  \item{showstructure}{ Show the directory structure in the console.}
}
\value{
  makeDirStruct returns the root of the experiment
}
\author{ Wouter D. Weeda}


\keyword{ methods }
