\name{arf2MNI}
\Rdversion{1.1}
\alias{arf2MNI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
arf2MNI(xyz_coor, registration)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xyz_coor}{
%%     ~~Describe \code{xyz_coor} here~~
}
  \item{registration}{
%%     ~~Describe \code{registration} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (xyz_coor, registration) 
{
    xyz = c(xyz_coor - 1, 1)
    examp_mm = .registration.Dex(registration) \%*\% xyz
    high_mm = .registration.Aex2hi(registration) \%*\% examp_mm
    high_vox = solve(.registration.Dhi(registration)) \%*\% high_mm
    high_vox_flipped = .registration.SXhi(registration) \%*\% high_vox
    high_mm_flipped = .registration.Dhi(registration) \%*\% high_vox_flipped
    stand_mm_flipped = .registration.Ahi2st(registration) \%*\% 
        high_mm_flipped
    stand_vox_flipped = solve(.registration.Dst(registration)) \%*\% 
        stand_mm_flipped
    stand_mni_flipped = .registration.OXst(registration) \%*\% 
        stand_vox_flipped
    return(stand_mni_flipped[-length(stand_mni_flipped)])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
