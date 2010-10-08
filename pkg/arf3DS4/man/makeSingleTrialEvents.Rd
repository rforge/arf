\name{makeSingleTrialEvents}
\alias{makeSingleTrialEvents}
\alias{makeRawTimeseries}

\title{
Estimate single-trial fMRI activity.
}
\description{
makeSingleTrialEvents calculates single-trial fMRI activity using a double gamma HRF.
}
\usage{
makeSingleTrialEvents(subject, condition, sefilename='single_events', hrf.control=list(a1=6,a2=12,b1=0.9,b2=0.9,ce=0.35), experiment=NULL)
}

\arguments{
  \item{subject}{Subject}
  \item{condition}{Condition}
  \item{sefilename}{Filename of the volume to be stored (stores a 4D file with a volume for each event).}
  \item{hrf.control}{A list containing the parameters for the double gamma .}
  \item{experiment}{experimentobject}
}
\details{
Files are stored in the data/functional directory. Use different filenames for different contrasts.
}
\value{
Returns a 4D nifti file (of class \code{\linkS4class{fmri.data}}) containing single-Trial events.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\linkS4class{fmri.data}}
}
\keyword{ models }
