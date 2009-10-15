\name{SpatialTemporalDataFrame}
\Rdversion{1.1}
\alias{SpatialTemporalDataFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SpatialTemporalDataFrame(stdf, location.col, time.col, tformat = "%Y-%m-%d")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stdf}{
%%     ~~Describe \code{stdf} here~~
}
  \item{location.col}{
%%     ~~Describe \code{location.col} here~~
}
  \item{time.col}{
%%     ~~Describe \code{time.col} here~~
}
  \item{tformat}{
%%     ~~Describe \code{tformat} here~~
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
function( stdf, location.col, time.col, tformat="\%Y-\%m-\%d"){
  ## Create a new stDF, given a full stdf, and the column numbers for location
  ## and time
  verbose <- FALSE
  require(sp)
  nrow <- nrow(stdf)
  ncol <- ncol(stdf)
  n.coord <- length(location.col)
  if ( any(location.col > ncol, time.col > ncol) ) {
    stop("indices for location and time columns must not be greater than the dataframe's ncol")
  }
  if (verbose) 
    pp("Extracting the locations and times from the data.frame...",Sys.time())
  locs <- stdf[,location.col]
  tims <- as.character(stdf[,time.col])
#  browser()
  s.id <- 1:nrow(unique(locs))  ## assumes locs is > 1!!! FIX!
  unique.locs <- unique(locs)
  if (verbose) 
    pp("Creating an stSpatialPoints object...",Sys.time())
  spsp <- SpatialPoints(unique(locs))
  sts <- new("stSpatialPoints", s.id=s.id, coords=spsp@coords, proj4string=spsp@proj4string, bbox=spsp@bbox)

  if (verbose) 
    pp("Creating an stTemporal object",Sys.time())
  unique.times <- timeDate(unique(tims), format=tformat)
  t.id <- 1:length(unique.times)
  stt <- new("stTemporal", t.id=t.id, timedatestamps=unique.times)

  ## construct stdf by removing the time and coordinate information.
  ## replace them with 2 new columns, s.id, t.id, and put in the corresponding
  ## id.
  ##
  ## match over unique.locs and unique.times

  ## ISSUE!
  ## match only seems to work on vectors...  so I coerce to character vector
  ## until somebody thinks of a better match for matrix rows...
  if (verbose) 
    pp("Determining unique IDs...",Sys.time())
  locs.as.char <- apply(locs,1,paste,collapse=" ")
  unique.locs.as.char <- apply(unique.locs,1,paste,collapse=" ")
  new.df.sid.col <- match( locs.as.char, unique.locs.as.char)
  new.df.tid.col <- match( as.character(timeDate(tims,format=tformat)), as.character(unique.times) )
  if (verbose) 
    pp("Creating an stDataFrame object",Sys.time())
  new.df <- data.frame( stdf[, -c(location.col, time.col)], new.df.tid.col, new.df.sid.col)  
  names(new.df) <- c(names(stdf)[-c(location.col, time.col)], "t.id","s.id")
  stdf.new <- new( "stDataFrame", s.id=s.id, t.id=t.id, df=new.df )
  if (verbose) 
    pp("Creating a SpatialTemporalDataFrame object",Sys.time())
  return( new("SpatialTemporalDataFrame", spatial=sts, temporal=stt, data=stdf.new) )
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
