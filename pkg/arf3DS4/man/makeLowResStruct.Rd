\name{makeLowResStruct}
\Rdversion{1.1}
\alias{makeLowResStruct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeLowResStruct(arfdata, experiment = .experiment)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{arfdata}{
%%     ~~Describe \code{arfdata} here~~
}
  \item{experiment}{
%%     ~~Describe \code{experiment} here~~
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
function (arfdata, experiment = .experiment) 
{
    trials = list.files(.data.regDir(arfdata), full = F)
    if (length(trials) == 0) 
        stop("No trial directories found found in", .data.regDir(arfdata), 
            ",please run registration.")
    for (trialdir in trials) {
        registration = loadRda(paste(.data.regDir(arfdata), .Platform$file.sep, 
            trialdir, .Platform$file.sep, .data.regRda(arfdata), 
            sep = ""))
        examp = readData(.registration.linkedfile(registration))
        highres = readData(paste(.registration.fullpath(registration), 
            .Platform$file.sep, .registration.highres(registration), 
            sep = ""))
        ex2high = readFSLmat(paste(.registration.fullpath(registration), 
            .Platform$file.sep, .registration.examp2high(registration), 
            sep = ""))
        highdat = .fmri.data.datavec(highres)
        dim(highdat) = c(.fmri.data.dims(highres)[2], .fmri.data.dims(highres)[3], 
            .fmri.data.dims(highres)[4])
        dimx = .fmri.data.dims(examp)[2]
        dimy = .fmri.data.dims(examp)[3]
        dimz = .fmri.data.dims(examp)[4]
        newdat = examp
        .fmri.data.filename(newdat) = .experiment.lowresFile(experiment)
        .fmri.data.fullpath(newdat) = paste(.data.regDir(arfdata), 
            .Platform$file.sep, trialdir, sep = "")
        newdatavec = rep(0, dimx * dimy * dimz)
        dim(newdatavec) = c(dimx, dimy, dimz)
        for (z in 1:dimz) {
            for (y in 1:dimy) {
                for (x in 1:dimx) {
                  xyz = c(x, y, z, 1)
                  examp_mm = .registration.Dex(registration) \%*\% 
                    xyz
                  high_mm = .registration.Aex2hi(registration) \%*\% 
                    examp_mm
                  high_vox = solve(.registration.Dhi(registration)) \%*\% 
                    high_mm
                  newdatavec[x, y, z] = highdat[high_vox[1], 
                    high_vox[2], high_vox[3]]
                }
            }
        }
        .fmri.data.cal_min(newdat) = min(newdatavec)
        .fmri.data.cal_max(newdat) = max(newdatavec)
        invisible(writeData(newdat, as.vector(newdatavec)))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
