\name{setFuncFiles}
\Rdversion{1.1}
\alias{setFuncFiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setFuncFiles(experiment = .experiment, func_data = "filtered_func_data.nii.gz")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{experiment}{
%%     ~~Describe \code{experiment} here~~
}
  \item{func_data}{
%%     ~~Describe \code{func_data} here~~
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
function (experiment = .experiment, func_data = "filtered_func_data.nii.gz") 
{
    sp <- .Platform$file.sep
    for (sdirs in 1:.experiment.subject.num(experiment)) {
        spath <- paste(.experiment.path(experiment), sp, .experiment.subjectDir(experiment), 
            sp, .experiment.subject.names(experiment)[sdirs], 
            sep = "")
        for (cdirs in 1:.experiment.condition.num(experiment)) {
            cpath <- paste(spath, sp, .experiment.conditionDir(experiment), 
                sp, .experiment.condition.names(experiment)[cdirs], 
                sp, .experiment.dataDir(experiment), sp, .experiment.funcDir(experiment), 
                sep = "")
            funcdirs <- list.files(cpath)
            for (trs in 1:length(funcdirs)) {
                functional <- loadRda(paste(cpath, sp, funcdirs[trs], 
                  sp, .experiment.funcRda(experiment), sep = ""))
                if (length(functional) > 0) {
                  .functional.fullpath(functional) <- paste(spath, 
                    sp, .experiment.funcDir(experiment), sp, 
                    funcdirs[trs], sep = "")
                  .functional.functionaldata(functional) <- func_data
                }
                else cat("[func] functional.Rda not found\n")
                save(functional, file = paste(.functional.filename(functional), 
                  sep = ""))
            }
        }
    }
    return(invisible(functional))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
