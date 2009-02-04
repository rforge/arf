\name{sims-class}
\docType{class}
\alias{sims-class}


\title{Class "sims" }
\description{	a 'sims' object contains information to perform simulations with ARF.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("sims", ...)}.

}
\section{Slots}{
	 \describe{
    \item{\code{numsims}:}{Number of simulations to perform. }
    \item{\code{numtrials}:}{Number of trials in each simulation. }
    \item{\code{theta}:}{Vector of parameters used to make the signal. }
    \item{\code{shapemodel}:}{Which shape model is used to create the signals (default is 'gauss') }
    \item{\code{noisemodel}:}{Which noise model is used to simulate the noise (Not yet implemented)}
    \item{\code{noiseFWHM}:}{Width of temporal smoothing kernel. }
    \item{\code{imageFWHM}:}{Width of spatial smoothing kernel.}
    \item{\code{sequence}:}{Sequence of models to fit at each simulation. }
    \item{\code{SNR}:}{Overall image signal-to-noise (temporal SNR is adjusted to obtain the correct image SNR). }
  }
}
\section{Methods}{
  \describe{
    \item{slots:}{Slots can be accessed via the standard accessor and replacment methods.}

	 }
}

\author{ Wouter D. Weeda }

\keyword{classes}
