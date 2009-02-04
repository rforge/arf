\name{data-class}
\docType{class}
\alias{data-class}


\title{Class "data"}
\description{The data class contains the information of the files belonging to a condition. Since fMRI files are usually large only the paths to the files (data, weights, averages) are stored in this class, with the number of trials. This class is used by \code{arf}.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("data", ...)}. Objects of class 'data' are automatically created by \code{arf} and only used internally. The class \code{\linkS4class{model}} inherits all slots from the 'data' class.
}
\section{Slots}{
	 \describe{
    \item{\code{name}:}{Name of the condition. (\code{"character"}) }
    \item{\code{fullpath}:}{Path to the rootdirectory of the condition (\code{"character"}) }
    \item{\code{datafiles}:}{Character vector of paths to the datafiles (in the 'data' directory') }
    \item{\code{weightfiles}:}{Character vector of paths to the weightfiles (in the 'weights' directory')  }
    \item{\code{avgdatfile}:}{Path to the averaged data (\code{"character"}) }
    \item{\code{avgWfile}:}{Path to the averaged weights (\code{"character"}) }
    \item{\code{trials}:}{Integer indicating the number of trials }
  }
}
\section{Methods}{
  \describe{
    \item{slots:}{Slots can be accessed via the standard accessor and replacment methods.}

	 }
}

\author{ Wouter D. Weeda }


\keyword{classes}

