\name{as.data.frame.SpatialTemporalDataFrame}
\Rdversion{1.1}
\alias{as.data.frame.SpatialTemporalDataFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
as.data.frame.SpatialTemporalDataFrame(from)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{from}{
%%     ~~Describe \code{from} here~~
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
function(from) {
        df.new <- getDataFrame(from)
        ## now replace the s.id and t.id with the actual values.
        new.temporal.col <- getTimedatestamps(from)[ match( df.new$t.id, getTid(from) ) ]
        new.spatial.col <- getSpatialPoints(from)[ match( df.new$s.id, getSid(from) ), ]
        df.names <- names(df.new)
        df.names <- df.names[ !(df.names \%in\% c("t.id","s.id")) ]
        return( data.frame( df.new[df.names], temporal=new.temporal.col, spatial=new.spatial.col) )
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
