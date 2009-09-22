\name{getAtlasLabels}
\Rdversion{1.1}
\alias{getAtlasLabels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getAtlasLabels(coordinates, registration, coortype = c("arf", "mni"), atlas = c("both", "Talairach", "HarvardOxford"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coordinates}{
%%     ~~Describe \code{coordinates} here~~
}
  \item{registration}{
%%     ~~Describe \code{registration} here~~
}
  \item{coortype}{
%%     ~~Describe \code{coortype} here~~
}
  \item{atlas}{
%%     ~~Describe \code{atlas} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (coordinates, registration, coortype = c("arf", "mni"), 
    atlas = c("both", "Talairach", "HarvardOxford"), ...) 
{
    if (!is.matrix(coordinates)) 
        stop("input coordinates must be in a matrix")
    if (ncol(coordinates) != 3) 
        stop("input coordinate matrix must have three columns (x,y,z)")
    atlas = match.arg(atlas)
    if (atlas == "both") {
        talairach = getTalairach(...)
        harvard = getHarvardOxford(...)
    }
    if (atlas == "Talairach") 
        talairach = getTalairach(...)
    if (atlas == "HarvardOxford") 
        harvard = getHarvardOxford(...)
    coortype = match.arg(coortype)
    if (coortype == "arf") 
        coordinates = t(apply(coordinates, 1, function(coordinates, 
            registration) arf2MNI(coordinates, registration), 
            registration = registration))
    coordinates = t(round(apply(coordinates, 1, function(coordinates, 
        registration) MNI2atlas(coordinates, registration), registration = registration)))
    atlasvec = vector(nrow(coordinates), mode = "list")
    for (i in 1:nrow(coordinates)) {
        if (atlas == "both") 
            atlasvec[[i]] = list(harvard = calcHarvardOxfordProbs(coordinates[i, 
                ], harvard), talairach = calcTalairach(coordinates[i, 
                ], talairach))
        if (atlas == "Talairach") 
            atlasvec[[i]] = list(talairach = calcTalairach(coordinates[i, 
                ], talairach))
        if (atlas == "HarvardOxford") 
            atlasvec[[i]] = list(harvard = calcHarvardOxfordProbs(coordinates[i, 
                ], harvard))
    }
    return(atlasvec)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
