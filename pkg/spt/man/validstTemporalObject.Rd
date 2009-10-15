\name{validstTemporalObject}
\Rdversion{1.1}
\alias{validstTemporalObject}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
validstTemporalObject(object)
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
  if(length(object@t.id) != length(object@timedatestamps))
    paste("Unequal t.id, timeDate lengths: ", length(object@t.id), ", ", length(object@timedatestamps), sep="")
  if ( !identical( class(object@t.id), "integer"))
    paste("The class of t.id must be 'integer', was given as '",class(object@t.id),"'",sep="")
  if ( !identical( class(object@timedatestamps)[[1]], "timeDate") )
    paste("The class of timedatestamps must be 'timeDate', was given as '",class(object@timedatestamps),"'",sep="")
  

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
