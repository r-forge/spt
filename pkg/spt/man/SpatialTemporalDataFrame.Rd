\name{SpatialTemporalDataFrame}
\Rdversion{1.1}
\alias{SpatialTemporalDataFrame}
\title{
  A constructor for class SpatialTemporalDataFrame
}
\description{
  Given a data.frame with columns corresponding to spatial location,
  time, and observation value and rows corresponding to observations, it
  returns a SpatialTemporalDataFrame object.
}
\usage{
SpatialTemporalDataFrame(stdf, location.col, time.col, tformat = "%Y-%m-%d")
}
\arguments{
  \item{stdf}{
    a data.frame or matrix (of characters)
}
\item{location.col}{
  the column indices of stdf which correspond to the locations, usually
  in x,y format, eg long/lat (so if col 2 is lat and col 3 is long, then
  you would use c(3,2)
}
\item{time.col}{
  the column index of stdf which corresponds to the time stamp (usually
  a character column)
}
  \item{tformat}{
    the format of the time column.  See the page for \code{\link{strptime}} for more
    information.  
}
}
\details{
see vignette
}
\value{
  returns an object of class SpatialTemporalDataFrame
}
\references{
see r-forge site.
}
\author{
Blair Christian
}
\note{
see vignette
}


\seealso{
  
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
