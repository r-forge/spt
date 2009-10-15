\name{validSpatialTemporalDataFrameObject}
\Rdversion{1.1}
\alias{validSpatialTemporalDataFrameObject}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
validSpatialTemporalDataFrameObject(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
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
function(object) {
  ## TBD
  ## Want to make sure that only the s.id and t.id locations are included in the dataframe...
  validObject(object@spatial)
  validObject(object@temporal)
  validObject(object@data)

  if ( !identical(sort(object@spatial@s.id), sort(object@data@s.id) ) )
    cat("unique spatial identifiers 's.id' must match in spatial and data objects")
  if ( !identical(sort(object@temporal@t.id), sort(object@data@t.id) ) )
    cat("unique temporal identifiers 't.id' must match in temporal and data objects")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
