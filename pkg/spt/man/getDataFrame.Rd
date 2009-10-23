\name{getDataFrame, setDataFrame}
\Rdversion{1.1}
\alias{getDataFrame}
\alias{setDataFrame}

\title{
Gets/sets data frame from a SpatialTemporalDataFrame or stDataFrame object
}
\description{
Gets/sets the data frame from a SpatialTemporalDataFrame or stDataFrame object.  Output/input the
data, unique spatial ids and unique temporal ids.
}
\usage{
getDataFrame(x)
setDataFrame(x) <- df
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
}
\references{  \url{http://spt.r-forge.r-project.org} }
\author{ J. Blair Christian, \email{blair.christian@gmail.com} }
\seealso{  \code{\link{}}}
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
