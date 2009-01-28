\name{wald-class}
\docType{class}
\alias{wald-class}


\title{Class "wald" ~~~ }
\description{ The Wald class contains information on the wald statistics and the designmatrix used.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("wald", ...)}.

}
\section{Slots}{
	 \describe{
    \item{\code{design}:}{Designmatrix containing the hypotheses.}
    \item{\code{stats}:}{Matrix of Wald statistic values.}
    \item{\code{df1}:}{Degrees of freedom (model) of the F-test }
    \item{\code{df2}:}{Degrees of freedom (error) of the F-test  }
    \item{\code{pvalues}:}{Matrix of p-values. }
  }
}
\section{Methods}{
  \describe{
    \item{slots:}{Slots can be accessed via the standard accessor and replacment methods.}

	 }
}
\references{  }
\author{ Wouter D. Weeda }
\note{  }

\seealso{ }
\examples{

}
\keyword{classes}
