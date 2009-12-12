
##.setUp <- function() {  ## called before each test case, see also .tearDown()
##  print(".setUp")
##}
checkTrue(require(methods))
checkTrue(require(fda))
checkTrue(require(fields))
checkTrue(require(timeDate))
checkTrue(require(sp))

test.SpatialPointsTemporalDataFrame <- function(){
  testClassName <- "SpatialPointsTemporalDataFrame"
  stdf <- data.frame( times=c("2008-09-29","2009-01-14","2012-12-12"), lat=c(31,33,33), long=c(-81,-80,-80), obs=c(50, 71, 10) )
  location.col <- 2:3
  time.col <- 1
  tmp <- SpatialPointsTemporalDataFrame(stdf, location.col, time.col)  
  checkTrue( is(tmp, testClassName))
  checkTrue( validObject(tmp))
  
}


test.subsetTemp <- function(){
  testClassName <- "SpatialPointsTemporalDataFrame"
  stdf <- data.frame( times=c("2008-09-29","2009-01-14","2012-12-12"), lat=c(31,33,33), long=c(-81,-80,-80), obs=c(50, 71, 10), stringsAsFactors=FALSE)
  location.col <- 2:3
  time.col <- 1
  tmp <- SpatialPointsTemporalDataFrame(stdf, location.col, time.col, format="%Y-%m-%d")    

  i <- timeDate(c("2007-01-10","2010-01-10") )
  tmp.sub <- stSubset(tmp, i)
  checkEquals( tmp.sub@temporal@t.id, 1:2)
  checkEquals( tmp.sub@spatial@s.id, 1:2)
  checkTrue( is(tmp.sub, testClassName))
  checkTrue( validObject(tmp.sub))
  
  i <- c("2007-01-10","2010-01-10")
  tmp.sub <- stSubset(tmp, i, format="%Y-%m-%d")  ## error here.
  checkEquals( tmp.sub@temporal@t.id, 1:2)
  checkEquals( tmp.sub@spatial@s.id, 1:2)
  checkTrue( is(tmp.sub, testClassName))
  checkTrue( validObject(tmp.sub))
  
  
  tmp.sub <- stSubset(tmp, as.integer(2), "temporal")
  checkEquals(getSid(tmp.sub), 2)
  checkEquals(getTid(tmp.sub), 2)  
  checkTrue( is(tmp.sub, testClassName))
  checkTrue( validObject(tmp.sub))    
}

test.subsetSpat <- function(){
  testClassName <- "SpatialPointsTemporalDataFrame"
  stdf <- data.frame( times=c("2008-09-29","2009-01-14","2012-12-12"), lat=c(31,33,33), long=c(-81,-80,-80), obs=c(50, 71, 10) )
  location.col <- 3:2
  time.col <- 1
  tmp <- SpatialPointsTemporalDataFrame(stdf, location.col, time.col)    

  tmp.sub <- stSubset(tmp, rbind(  c(-80.5,-79), c(32,34)) )

  checkEquals( getSid(tmp.sub), 2)
  checkEquals( as.numeric(coordinates(getstSpatial(tmp.sub))), c(-80,33) )
  checkTrue( is(tmp.sub, testClassName))
  checkTrue( validObject(tmp.sub))

  ## Now check subsetting for integer 2nd arg...
  stdf <- data.frame( times=c("2008-09-29","2008-12-14","2012-12-12","2012-12-12"), lat=c(31,33,33,31), long=c(-81,-80,-80,-81), obs=c(50, 71, 10, 20) )
  location.col <- 2:3
  time.col <- 1
  tmp.stdf <- SpatialPointsTemporalDataFrame(stdf, location.col, time.col, format="%Y-%m-%d")
  
  tmp.sub <- stSubset(tmp.stdf, as.integer(2), "spatial")
  checkEquals(getSid(tmp.sub), 2)
  checkEquals(getTid(tmp.sub), 2:3)  ## error here
  checkTrue( is(tmp.sub, testClassName))
  checkTrue( validObject(tmp.sub))    
}


test.stDist <- function(){
  testClassName <- "SpatialPointsTemporalDataFrame"
  stdf <- data.frame( times=c("2008-09-29","2009-01-14","2012-12-12"), lat=c(31,33,33), long=c(-81,-80,-80), obs=c(50, 71, 10) )
  location.col <- 3:2
  time.col <- 1
  tmp <- SpatialPointsTemporalDataFrame(stdf, location.col, time.col)    

  load("teststtemporal.Rdata")
  checkEquals(sum(stDist(tmp,type="temporal",units="days")-stdistDays),0)
  checkEquals(sum(stDist(tmp,"temporal","weeks")-stdistWeeks),0)

#  stDist(tmp,type="spatial",units="earth",miles=FALSE)
  require(fields)
  load("testspatialpointsdataframe.Rdata")
  checkEquals(sum(stDist(tmp,"spatial","euclidean")- stdisteu),0)
  checkTrue(sum(stDist(tmp,"spatial","earth")- stdistea)< .1)
  
  checkException(stDist(tmp,"blue"))
}

test.getTimeBySpaceMat <- function(){
  testClassName <- "SpatialPointsTemporalDataFrame"
  stdf <- data.frame( times=c("2008-09-29","2008-12-14","2012-12-12","2012-12-12"), lat=c(31,33,33,31), long=c(-81,-80,-80,-81), obs=c(50, 71, 10, 20) )
  location.col <- 3:2
  time.col <- 1
  tmp <- SpatialPointsTemporalDataFrame(stdf, location.col, time.col)
  denseMat <- getTimeBySpaceMat(tmp, "obs")
  checkMat <- cbind(c(50,NA,20),c(NA,71,10) )
  colnames(checkMat) <- paste("sid",1:2,sep="_")
  rownames(checkMat) <- paste("tid",1:3,sep="_")
  checkEquals(denseMat, checkMat)
}

test.stJoin <- function(){
  testClassName <- "SpatialPointsTemporalDataFrame"
  stdf <- data.frame( times=c("2008-09-29","2008-12-14","2012-12-12","2012-12-12"), lat=c(31,33,33,31), long=c(-81,-80,-80,-81), obs=c(50, 71, 10, 20) )
  location.col <- 3:2
  time.col <- 1
  s1 <- SpatialPointsTemporalDataFrame(stdf, location.col, time.col)

  stdf <- data.frame( times=c("2008-09-29","2008-09-30","2012-12-12","2020-01-12"), lat=c(31,33,33,31), long=c(-81,-80,-80,-81), obs=c(51, 72, 11, 21) )
  location.col <- 3:2
  time.col <- 1
  s2 <- SpatialPointsTemporalDataFrame(stdf, location.col, time.col)
  
  j <- stJoin(s1,s2)

  checkEquals(getTid(j), 1:5)
  checkEquals(getSid(j), 1:2)
  checkEquals(getDataFrame(j)$obs, c(50,71,10,20,51,72,11,21) )
  checkTrue( is(j, testClassName))
  checkTrue( validObject(j))    
}

test.stApply <- function(){
  stdf <- data.frame( times=c("2008-09-29","2008-12-14","2012-12-12","2012-12-12"), lat=c(31,33,33,31), long=c(-81,-80,-80,-81), obs=c(50, 71, 10, 20) )
  location.col <- 2:3
  time.col <- 1
  tmp.stdf <- SpatialPointsTemporalDataFrame(stdf, location.col, time.col)

  
}


test.utilities <- function(){
  pollutant <- "Oz"
  state <- "GA"
  startDate <- "2007-01-01"
  endDate <- "2007-01-30"
  checkException(getEPAAirExplorerData(pollutant, state, startDate, endDate))
  
}
