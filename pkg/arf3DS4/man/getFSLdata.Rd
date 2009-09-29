\name{getFSLdata}
\Rdversion{1.1}
\alias{getFSLdata}
\title{ Read and copy data from a FSL FEAT directory }
\description{getFSLdata reads and copies data from specified subjects to an arf3DS4 experiment.   
}
\usage{
getFSLdata(fsldir = getwd(), subjectlist, contrastnums, expname = "FSLtoARF", expdir = getwd(), settings = new("settings"), quiet = F, doReg = T, doFunc = F)
}
\arguments{
  \item{fsldir}{Path of the FSL FEAT directory.}
  \item{subjectlist}{Vector of subjectnames (excluding the \code{.feat}) to be copied.}
  \item{contrastnums}{Vector of numbers of contrasts (COPE/VARCOPE numbers).}
  \item{expname}{Name of the (to be created) arf experiment to which the files will be copied.}
  \item{expdir}{Path of the directory where the experiment will be saved.}
  \item{settings}{A "settings" object (see \code{\linkS4Class{settings}}).}
  \item{quiet}{Should progress information be printed during copying?}
  \item{doReg}{Copy optional registration files?}
  \item{doFunc}{Copy optional functional volumes?}
}
\details{
getFSLdata checks for additional FEAT directories (.feat+ etc.) and takes each directory as an individual trial. It also creates and sets appropriate \code{\linkS4Class{registration}} and \code{\linkS4Class{functional}} objects (if called for).
}
\value{
Returns an object of class "experiment" \code{\linkS4Class{experiment}}
}
\references{
See \url{http://www.fmrib.ox.ac.uk/fsl/} on how to obtain FSL and the appropriate licences.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{experiment}}, \code{\linkS4class{settings}}, \code{\linkS4class{registration}}, \code{\linkS4class{functional}}
}
\keyword{ models }
