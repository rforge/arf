\name{clearWarnings}
\Rdversion{1.1}
\alias{clearWarnings}
\title{ Clear warnings of a model object.}
\description{
Clearwarnings clears all warnings of a model object and sets the valid slot to \code{TRUE}. It asks for confirmation.
}
\usage{
clearWarnings(arfmodel, resetValid = T)
}
\arguments{
  \item{arfmodel}{Object of class "model" (see \code{\linkS4class{model}})}
  \item{resetValid}{Set the \code{valid} slot to \code{TRUE}}
}
\details{
Warnings are only cleared when starting the minimization procedure on a model object. After minimization all warnings are added to the \code{warnings} slot.
}
\value{
None
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{model}}
}
\keyword{ models }

