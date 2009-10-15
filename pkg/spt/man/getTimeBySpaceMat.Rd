\name{getTimeBySpaceMat}
\Rdversion{1.1}
\alias{getTimeBySpaceMat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getTimeBySpaceMat(st, colname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{st}{
%%     ~~Describe \code{st} here~~
}
  \item{colname}{
%%     ~~Describe \code{colname} here~~
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
function(st,colname){
            ## FIX TBD
            ## Assumes that time is stored in order????
            n.t <- length(getTid(st))
            n.s <- length(getSid(st))
            dens <- matrix(NA, nrow= n.t, ncol=n.s)
            rownames(dens) <- paste("tid",getTid(st),sep="_")
            colnames(dens) <- paste("sid",getSid(st),sep="_")
            id.pairs <- getDataFrame(st)[c("s.id","t.id")]
            for (i in 1:n.t){
              for (j in 1:n.s){
                tidMatches <- id.pairs$"t.id" == getTid(st)[i]
                sidMatches <- id.pairs$"s.id" == getSid(st)[j]
                if ( sum( tidMatches & sidMatches)==1){
                  dens[i,j] <- getDataFrame(st)[ tidMatches & sidMatches, colname]
                }
              }
            }
            return(dens)
          }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
