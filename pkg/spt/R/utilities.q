
getEPAAirExplorerData <- function(pollutant, state, startDate, endDate,tformat="%Y-%m-%d"){
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
    sy <- format(dateStart,"%Y")
    sm <- format(dateStart,"%m")
    sd <- format(dateStart,"%d")
    ey <- format(dateEnd,"%Y")
    em <- format(dateEnd,"%m")
    ed <- format(dateEnd,"%d")
    aeAction <- "http://www.epa.gov/cgi-bin/broker"
    stateVal <- getStateNumber(state)
    pollVal <- getPollutantNumber(pollutant)
    msaocV <- paste("%22","%22",sep=stateVal)
    aeData <- getForm(aeAction, msaorcountyName="statecode", msaorcountyValue=msaocV, poll=pollVal,county="-1", site="-1", msa="-1",state=stateVal, sm=sm,sd=sd,sy=sy,em=em,ed=ed,ey=ey,flag="Y", query="download", "_debug"="2", "_service"="data", "_program"="dataprog.query_daily3P.sas", binary=FALSE)
    if (class(aeData)=="raw") aeData <- rawToChar(aeData) # to fix platform dep issue?
    if ( length(grep("No observations",aeData))>0 ){
      return(NA)
    }
    con <- textConnection(aeData)
    aeMatrix <- read.csv(con,as.is=TRUE)[, c("Date","SITE","Concentration","LATITUDE","LONGITUDE","ELEVATION")]
    close(con)
    rm("aeData"); gc()
    return(SpatialTemporalDataFrame(stdf=aeMatrix, location.col=5:4, time.col=1, tformat="%m/%d/%Y") )
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

color.matlab= c("#00008F", "#00009F", "#0000AF", "#0000BF", "#0000CF", "#0000DF",
  "#0000EF", "#0000FF", "#0010FF", "#0020FF", "#0030FF", "#0040FF",
  "#0050FF", "#0060FF", "#0070FF", "#0080FF", "#008FFF", "#009FFF",
  "#00AFFF", "#00BFFF", "#00CFFF", "#00DFFF", "#00EFFF", "#00FFFF",
  "#10FFEF", "#20FFDF", "#30FFCF", "#40FFBF", "#50FFAF", "#60FF9F",
  "#70FF8F", "#80FF80", "#8FFF70", "#9FFF60", "#AFFF50", "#BFFF40",
  "#CFFF30", "#DFFF20", "#EFFF10", "#FFFF00", "#FFEF00", "#FFDF00",
  "#FFCF00", "#FFBF00", "#FFAF00", "#FF9F00", "#FF8F00", "#FF8000",
  "#FF7000", "#FF6000", "#FF5000", "#FF4000", "#FF3000", "#FF2000",
  "#FF1000", "#FF0000", "#EF0000", "#DF0000", "#CF0000", "#BF0000",
  "#AF0000", "#9F0000", "#8F0000", "#800000")

pp <- function(...) print(paste(...))
