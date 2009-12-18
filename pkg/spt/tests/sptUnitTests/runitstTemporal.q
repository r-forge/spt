
##.setUp <- function() {  ## called before each test case, see also .tearDown()
##  print(".setUp")
##}
checkTrue(require(methods))

test.stTemporal <- function(){
  testClassName <- "stTemporal"
  this <- stTemporal(timeVals=c("2008-09-29","2009-01-14","2012-12-12"),format="%Y-%m-%d")
  checkTrue( is(this, testClassName))
  checkTrue( validObject(this))
}


test.stDist <- function(){
  this <- stTemporal(timeVals=c("2008-09-29","2009-01-14","2012-12-12"),format="%Y-%m-%d")
  load("teststtemporal.Rdata")
  checkEquals(sum(stDist(this,"days")-stdistDays),0)
  checkEquals(sum(stDist(this,"weeks")- stdistWeeks),0)
}


test.stUpdate <- function(){
  testClassName <- "stTemporal"
  to.keep <- as.integer(1:2)
  tmp <- stTemporal(timeVals=c("2008-09-29","2009-01-14","2012-12-12"),format="%Y-%m-%d")
  tmp.up <- stUpdate(tmp, to.keep )
  checkEquals( tmp.up@t.id, to.keep)
  checkEquals( tmp.up@timedatestamps, timeDate(c("2008-09-29","2009-01-14"),format="%Y-%m-%d"))
  checkTrue( is(tmp.up, testClassName))
  checkTrue( validObject(tmp.up))
}


test.stSubset <- function(){
  testClassName <- "stTemporal"
  tmp <- stTemporal(timeVals=c("2008-09-29","2009-01-14","2012-12-12"),format="%Y-%m-%d")

  for (i in list(timeDate(c("2007-01-10","2010-01-10") ), c("2007-01-10","2010-01-10") )){
    if( class(i)=="character"){
      tmp.sub <- stSubset(tmp, i,format="%Y-%m-%d")
    } else {
      tmp.sub <- stSubset(tmp, i)
    }
    checkEquals( tmp.sub@t.id, 1:2)
    checkTrue( is(tmp.sub, testClassName))
    checkTrue( validObject(tmp.sub))
  }

  format <- "%Y"
  tmp.sub <- stSubset(tmp, "2009", format)
  checkEquals( tmp.sub@t.id, as.integer(2) )

  
  tmp.sub <- stSubset(tmp, as.integer(2) )
  checkEquals( tmp.sub@t.id, as.integer(2) )

  tmp.sub <- stSubset(tmp, 2.0 )
  checkEquals( tmp.sub@t.id, as.integer(2) )
  checkException( stSubset(tmp, 2.2 ))
}

test.getTimedatestamps <- function(){
  testClassName <- "stTemporal"
  this <- stTemporal(timeVals=c("2008-09-29","2009-01-14","2012-12-12"),format="%Y-%m-%d")

  format <- "%Y"
  ftest <- getTimedatestamps(this, format)
  checkEquals(ftest, as.character(c(2008,2009,2012)))

  format <- "%m"
  ftest <- getTimedatestamps(this, format)
  checkEquals(ftest, c("09","01","12"))

  ftest <- getTimedatestamps(this)
  checkEquals(ftest[1], "2008-09-29")

  format <- "%m/%d/%Y"
  ftest <- getTimedatestamps(this,format)
  checkEquals(ftest[1], "09/29/2008")

  this <- stTemporal(timeVals=c("2008-09-29","2009-01-14","2012-12-12"),format="%Y-%m-%d")
  format <- "%Y-%m-%d"
  ftest <- getTimedatestamps(this, y=as.integer(c(3,1,2)) ,format)
  checkEquals(ftest, c("2012-12-12", "2008-09-29", "2009-01-14"))
  
}
  
test.stGetDataFrame <- function(){
  testClassName <- "stTemporal"
  t1 <- stTemporal(timeVals=c("2008-09-29","2009-01-14","2012-12-12"),format="%Y-%m-%d")
  df1 <- data.frame( tid=as.integer(1:3), timedatestamps=c("2008-09-29","2009-01-14","2012-12-12") )
  checkEquals(getDataFrame(t1), df1)

  df2 <- data.frame( tid=as.integer(1:3), timedatestamps=c("09/29/2008","01/14/2009","12/12/2012") )
  checkEquals(getDataFrame(t1,format = "%m/%d/%Y"), df2)

}

test.stJoin <- function(){
  testClassName <- "stTemporal"
  t1 <- stTemporal(timeVals=c("2008-09-29","2009-01-14","2012-12-12"),format="%Y-%m-%d")
  t2 <- stTemporal(timeVals=c("2008-09-29","2009-01-14","2012-12-12","2020-01-11"),format="%Y-%m-%d")

  j <- stJoin(t1,t2)
  checkEquals(getTimedatestamps(j$stTemporal), getTimedatestamps(t2))
  checkEquals(j$x.id.map, 1:3)
  checkEquals(j$y.id.map, 1:4)
  checkTrue( is(j$stTemporal, testClassName))
  checkTrue( validObject(j$stTemporal))


  t1.a <- stTemporal(timeVals=c("2008-09-29","2009-01-14","2012-12-12"),format="%Y-%m-%d")
  t2.a <- stTemporal(timeVals=c("2008-09-29","2020-01-11"),format="%Y-%m-%d")

  j <- stJoin(t1.a,t2.a)
  checkEquals(getTimedatestamps(j$stTemporal), getTimedatestamps(t2))
  checkEquals(j$x.id.map, 1:3 )
  checkEquals(j$y.id.map, c(1,4))
  checkTrue( is(j$stTemporal, testClassName))
  checkTrue( validObject(j$stTemporal))


  t1.b <- stTemporal(timeVals=c("2008-09-29","2009-01-14","2012-12-12"),format="%Y-%m-%d")
  t2.b <- stTemporal(timeVals=c("2008-09-30","2020-01-11"),format="%Y-%m-%d")

  j <- stJoin(t1.b, t2.b)
  checkEquals(getTimedatestamps(j$stTemporal), c("2008-09-29", "2008-09-30", "2009-01-14", "2012-12-12", "2020-01-11"))
  checkEquals(j$x.id.map, c(1,3,4) )
  checkEquals(j$y.id.map, c(2,5))
  checkTrue( is(j$stTemporal, testClassName))
  checkTrue( validObject(j$stTemporal))

  
  t1.c <- new("stTemporal",
              timedatestamps=timeDate(c("2008-09-29","2012-12-12","2009-01-14")),
              t.id = as.integer(5:7), timeFormat="%Y-%m-%d")
  t2.c <- new("stTemporal",
              timedatestamps=timeDate(c("2020-01-11","2008-09-30")),
              t.id=as.integer(10:11), timeFormat="%Y-%m-%d")

  j <- stJoin(t1.c, t2.c)
  checkEquals(getTimedatestamps(j$stTemporal), c("2008-09-29", "2008-09-30", "2009-01-14", "2012-12-12", "2020-01-11"))
  checkEquals(j$x.id.map, c(1,4,3) )
  checkEquals(j$y.id.map, c(5,2))
  checkTrue( is(j$stTemporal, testClassName))
  checkTrue( validObject(j$stTemporal))
}
