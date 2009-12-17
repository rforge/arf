\name{arf2MNI}
\Rdversion{1.1}
\alias{arf2MNI}
\alias{MNI2arf}
\alias{MNI2atlas}
\alias{arf2high}


\title{Convert arf-coordinates to MNI152 coordinates and vice-versa}
\description{
Converts arf-coordinates of a region to MNI152 coordinates and back, also convert MNI coordinates to MNI atlas coordinates and arf-coordinates to high-resolution coordinates. The function needs an object of class "registration" (\code{\linkS4class{registration}}) from the data the arf-model was fitted to.
}
\usage{
arf2MNI(xyz_coor, registration)
MNI2arf(xyz_coor, registration)
MNI2atlas(xyz_coor, registration)
arf2high(xyz_coor, registration)

}
\arguments{
  \item{xyz_coor}{Coordinate triple (x,y,z), containing coordinates to be converted.}
  \item{registration}{An object of class "registration"(\code{\linkS4class{registration}}).}
}
\details{
When using \code{\link{getFSLdata}} to setup the experiment, the registration objects are filled automatically when \code{doRegs = TRUE}.
}
\value{
Returns a vector of length (3) containing the coordinates in the appropriate space.
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{ \code{\linkS4class{registration}},\code{\link{getFSLdata}} }
\keyword{ methods }

