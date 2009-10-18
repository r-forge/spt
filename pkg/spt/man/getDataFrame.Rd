\name{getDataFrame}
\Rdversion{1.1}
\alias{getDataFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets data frame from a SpatialTemporalDataFrame or stDataFrame object
}
\description{
Gets data frame from a SpatialTemporalDataFrame or stDataFrame object.  Returns the
data, unique spatial ids and unique temporal ids.
}
\usage{
getDataFrame(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of class SpatialTemporalDataFrame or stDataFrame 
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{df} {a data.frame with 3 or more columns; a unique spatial id, a
  unique temporal id, and one or more columns of observed values}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  \url{http://spt.r-forge.r-project.org}
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
function(x){ standardGeneric("getDataFrame")}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
