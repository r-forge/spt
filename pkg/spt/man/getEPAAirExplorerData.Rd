\name{getEPAAirExplorerData}
\Rdversion{1.1}
\alias{getEPAAirExplorerData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getEPAAirExplorerData(pollutant, state, startDate, endDate, tformat = "%Y-%m-%d")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pollutant}{
%%     ~~Describe \code{pollutant} here~~
}
  \item{state}{
%%     ~~Describe \code{state} here~~
}
  \item{startDate}{
%%     ~~Describe \code{startDate} here~~
}
  \item{endDate}{
%%     ~~Describe \code{endDate} here~~
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
function(pollutant, state, startDate, endDate,tformat="\%Y-\%m-\%d"){
  getPollutantNumber <- function(pollutant){
    poll.names <- c("Pb", "CO", "SO2", "NO2", "Ozone", "PM10", "PM2.5")
    poll.vals <- as.character(c( 12128, 42101, 42104, 42602, 44201, 81102, 88101))
    curr.poll <- match.arg(pollutant, poll.names)
    return( poll.vals[ which(curr.poll==poll.names)])
  }
  getStateNumber <- function(state){
    state.vals <-  c(  "01","02","04","05","06","08","09","10","11","12","13","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","44","45","46","47","48","49","50","51","53","54","55","56","72","78")
    state.abb <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","PR","VI")
    curr.state <- match.arg(state, state.abb)
    return( state.vals[ which(curr.state==state.abb)])
  }
  getStateData <- function(pollutant, state, startDate, endDate, tformat){
    dateStart <- timeDate(startDate,tformat)
    dateEnd <- timeDate(endDate,tformat)
    sy <- format(dateStart,"\%Y")
    sm <- format(dateStart,"\%m")
    sd <- format(dateStart,"\%d")
    ey <- format(dateEnd,"\%Y")
    em <- format(dateEnd,"\%m")
    ed <- format(dateEnd,"\%d")
    aeAction <- "http://www.epa.gov/cgi-bin/broker"
    stateVal <- getStateNumber(state)
    pollVal <- getPollutantNumber(pollutant)
    msaocV <- paste("\%22","\%22",sep=stateVal)
    aeData <- getForm(aeAction, msaorcountyName="statecode", msaorcountyValue=msaocV, poll=pollVal,county="-1", site="-1", msa="-1",state=stateVal, sm=sm,sd=sd,sy=sy,em=em,ed=ed,ey=ey,flag="Y", query="download", "_debug"="2", "_service"="data", "_program"="dataprog.query_daily3P.sas", binary=FALSE)
    if (class(aeData)=="raw") aeData <- rawToChar(aeData) # to fix platform dep issue?
    if ( length(grep("No observations",aeData))>0 ){
      return(NA)
    }
    con <- textConnection(aeData)
    aeMatrix <- read.csv(con,as.is=TRUE)[, c("Date","SITE","Concentration","LATITUDE","LONGITUDE","ELEVATION")]
    close(con)
    rm("aeData"); gc()
    return(SpatialTemporalDataFrame(stdf=aeMatrix, location.col=5:4, time.col=1, tformat="\%m/\%d/\%Y") )
  }
  n.state <- length(state)
  if ( n.state==1){
    print(paste("getting data for state",state,"..."))
    curr.state <- getStateData(pollutant, state[1], startDate,endDate, tformat)
      if ( !identical(NA,curr.state)) {
        return(curr.state)
      } else {
        stop(paste("There is no data for this pollutant in the time interval for state:",state[1]))
      }
  } else {
    spt <- NA
    cnt <- 1
    while ( identical(NA,spt) & cnt <= n.state ){
      print(paste("getting data for state",state[cnt],"..."))
      spt <- getStateData(pollutant, state[cnt], startDate,endDate, tformat)
      if ( identical(NA,spt))
        print(paste("There is no data for this pollutant in this time interval for state:",state[cnt]))
    }
    
    if (cnt == n.state){
      stop("No data match the criteria")
    } else {
      for (i in cnt:n.state){
        print(paste("getting data for state",state[i],"..."))
        curr.state <- getStateData(pollutant, state[i], startDate,endDate, tformat)
        if ( !identical(NA,curr.state)) {
          spt <- stJoin(spt, curr.state)
        } else {
          print(paste("There is no data for this pollutant in the time interval for state:",state[i]))
        }
      }
      return(spt)
    }
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
