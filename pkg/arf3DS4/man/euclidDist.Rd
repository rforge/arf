\name{euclidDist}
\Rdversion{1.1}
\alias{euclidDist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
euclidDist(arfmodel, thres = 5, quiet = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{arfmodel}{
%%     ~~Describe \code{arfmodel} here~~
}
  \item{thres}{
%%     ~~Describe \code{thres} here~~
}
  \item{quiet}{
%%     ~~Describe \code{quiet} here~~
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
function (arfmodel, thres = 5, quiet = T) 
{
    theta = matrix(.model.estimates(arfmodel), 10)
    p = 0
    for (i in 1:(ncol(theta) - 1)) {
        for (j in (i + 1):ncol(theta)) {
            dist = 0
            for (k in 1:3) dist = dist + (theta[k, i] - theta[k, 
                j])^2
            distmat[i, j] = c(sqrt(dist))
            opp = FALSE
            if (theta[10, j] < 0 & theta[10, i] > 0) 
                opp = TRUE
            if (theta[10, j] > 0 & theta[10, i] < 0) 
                opp = TRUE
            if (i != j & distmat[i, j] < thres & opp) {
                if (!quiet) 
                  cat("region", i, "[", round(theta[c(1, 2, 3, 
                    4, 5, 6, 10), i]), "] -", j, "[", round(theta[c(1, 
                    2, 3, 4, 5, 6, 10), j]), "] distance:", sqrt(dist), 
                    "\n")
            }
            p = p + 1
        }
    }
    return(distmat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
