\name{getModelAtlas}
\Rdversion{1.1}
\alias{getModelAtlas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getModelAtlas(arfmodel, regtrial = 1, saveastext = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{arfmodel}{
%%     ~~Describe \code{arfmodel} here~~
}
  \item{regtrial}{
%%     ~~Describe \code{regtrial} here~~
}
  \item{saveastext}{
%%     ~~Describe \code{saveastext} here~~
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
function (arfmodel, regtrial = 1, saveastext = F) 
{
    sp <- .Platform$file.sep
    trials = list.files(.model.regDir(arfmodel), full = F)[regtrial]
    registration = loadRda(paste(.model.regDir(arfmodel), sp, 
        trials, sp, .model.regRda(arfmodel), sep = ""))
    estimates = matrix(.model.estimates(arfmodel), .model.params(arfmodel))
    coors = t(round(estimates[c(1, 2, 3), ]))
    atlas = getAtlasLabels(coors, registration, coortype = "arf")
    if (saveastext) 
        sink(paste(.model.modelname(arfmodel), "-atlas.txt", 
            sep = ""), split = T)
    for (i in 1:nrow(coors)) {
        cat("Region ", i, " [", coors[i, 1], " ", coors[i, 2], 
            " ", coors[i, 3], "] @ ", round(estimates[10, i]), 
            "\n", sep = "")
        cat("HarvardOxford:\n")
        for (j in 1:length(atlas[[i]]$harvard)) cat("  ", atlas[[i]]$harvard[j], 
            "\n")
        cat("Talairach:\n")
        for (j in 1:length(atlas[[i]]$talairach)) cat("  ", atlas[[i]]$talairach[j], 
            "\n")
        cat("\n")
    }
    if (saveastext) 
        sink()
    return(invisible(atlas))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
