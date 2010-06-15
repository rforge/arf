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
makeSingleTrialEvents(arfmodel, sefilename = "single_events")
}

\arguments{
  \item{arfmodel}{An arf model object (class \code{\linkS4class{model}})}
  \item{sefilename}{Filename of the volume to be stored (stores a 4D file with a volume for each event).}
}
\details{
Files are stored in the data/functional directory. Use different filenames for different contrasts.
}
\value{
Returns a 4D nifti file (of class \code{\linkS4class{fmri.data}}) containing single-event estimates.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}, \code{\linkS4class{fmri.data}}
}
\keyword{ models }
