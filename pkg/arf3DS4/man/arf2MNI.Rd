\name{arf2MNI}
\Rdversion{1.1}
\alias{arf2MNI}
\title{Convert arf-coordinates to MNI152 coordinates}
\description{
arf2MNI converts arf-coordinates of a region to MNI152 coordinates. The function needs an object of class "registration" (\code{\linkS4class{registration}}) from the data the arf-model was fitted to.
}
\usage{
arf2MNI(xyz_coor, registration)
}
\arguments{
  \item{xyz_coor}{Coorindate triple (x,y,z), containing coordinates to be converted.}
  \item{registration}{An object of class "registration"(\code{\linkS4class{registration}}).}
}
\details{
When using \code{\link{getFSLdata}} to setup the experiment, the registration objects are filled automatically when \code{doRegs = TRUE}.
}
\value{
Returns a vector of length (3) containing the coordinates in MNI152 space.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{ \code{\linkS4class{registration}},\code{\link{getFSLdata}} }
\keyword{ methods }

