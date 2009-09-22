\name{makeLowResStructAvg}
\Rdversion{1.1}
\alias{makeLowResStructAvg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeLowResStructAvg(arfmodel, experiment = .experiment)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{arfmodel}{
%%     ~~Describe \code{arfmodel} here~~
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
function (arfmodel, experiment = .experiment) 
{
    sp = .Platform$file.sep
    trials = list.files(.model.regDir(arfmodel), full = F)
    avgdat = 0
    for (trialdir in trials) {
        registration = loadRda(paste(.model.regDir(arfmodel), 
            sp, trialdir, sp, .model.regRda(arfmodel), sep = ""))
        fn = list.files(path = .registration.fullpath(registration), 
            pattern = .experiment.lowresFile(experiment), full = T)
        lrdat = readData(fn[1])
        avgdat = avgdat + .fmri.data.datavec(lrdat)
    }
    avgdat = avgdat/length(trials)
    avgstruct = lrdat
    .fmri.data.filename(avgstruct) = .experiment.lowresAvg(experiment)
    .fmri.data.fullpath(avgstruct) = .model.modeldatapath(arfmodel)
    .fmri.data.cal_min(avgstruct) = min(avgdat)
    .fmri.data.cal_max(avgstruct) = max(avgdat)
    invisible(writeData(avgstruct, avgdat))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
