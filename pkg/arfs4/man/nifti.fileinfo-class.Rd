\name{nifti.fileinfo-class}
\docType{class}
\alias{nifti.fileinfo-class}


\title{Class "nifti.fileinfo" ~~~ }
\description{	 nifti.fileinof contains information necessary to open NIFTI/ANALYZE files.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("nifti.fileinfo", ...)}.
	 
}
\section{Slots}{
	 \describe{
    \item{\code{fullpath}:}{Path of where the file is located.}
    \item{\code{filename}:}{Filename (without extension). }
    \item{\code{filetype}:}{NIFTI (.nii or .hdr/.img pair) or ANALYZE (.hdr/.img pair).}
    \item{\code{extension}:}{.nii or .hdr }
    \item{\code{gzipped}:}{Is the file gzipped?}
    \item{\code{endian}:}{Endianness. }
  }
}
\section{Methods}{
  \describe{
    \item{slots:}{Slots can be accessed via the standard accessor and replacment methods.}

	 }
}
\references{  }
\author{ Wouter D. Weeda }
\note{ }

\seealso{ \code{"\linkS4class{nifti.fileinfo}"} }
\examples{

}
\keyword{classes}
