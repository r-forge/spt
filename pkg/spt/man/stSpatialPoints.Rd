\name{stSpatialPoints}
\Rdversion{1.1}
\alias{stSpatialPoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stSpatialPoints(coords, bbox = matrix(NA), proj4string = CRS(as.character(NA)), s.id = as.integer(1:nrow(coords)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coords}{
%%     ~~Describe \code{coords} here~~
}
  \item{bbox}{
%%     ~~Describe \code{bbox} here~~
}
  \item{proj4string}{
%%     ~~Describe \code{proj4string} here~~
}
  \item{s.id}{
%%     ~~Describe \code{s.id} here~~
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
function(coords, bbox=matrix(NA), proj4string = CRS(as.character(NA)), s.id=as.integer(1:nrow(coords)) ){
  spsp <- SpatialPoints(coords, proj4string, bbox)
  return(new("stSpatialPoints", s.id=s.id, coords=as.matrix(spsp@coords), bbox=spsp@bbox, proj4string = spsp@proj4string))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
