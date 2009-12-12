
##.setUp <- function() {  ## called before each test case, see also .tearDown()
##  print(".setUp")
##}
checkTrue(require(methods))

test.stDataFrame <- function(){
  testClassName <- "stDataFrame"
  this <- new("stDataFrame", s.id=as.integer(1:3), t.id=as.integer(1:3),
             df=data.frame( s.id=as.integer(1:3), t.id=as.integer(1:3), x1=runif(3), x2=rnorm(3)))
  checkTrue( is(this, testClassName))
  checkTrue( validObject(this))
}


test.stUpdateTemp <- function(){
  testClassName <- "stDataFrame"

  tmp <- new("stDataFrame", s.id=as.integer(1:3), t.id=as.integer(1:3),
              df=data.frame( s.id=as.integer(1:3), t.id=as.integer(1:3), x1=runif(3), x2=rnorm(3)))

  to.keep <- as.integer(2:3)
  tmp.t <- stUpdate(tmp, to.keep, type="temporal")

  checkEquals( tmp.t@t.id, to.keep)
  checkEquals( tmp.t@s.id, to.keep)
  checkTrue( is(tmp.t, testClassName))
  checkTrue( validObject(tmp.t))
}

test.stUpdateSpat <- function(){
  testClassName <- "stDataFrame"
  tmp <- new("stDataFrame", s.id=as.integer(1:3), t.id=as.integer(1:3),
              df=data.frame( s.id=as.integer(1:3), t.id=as.integer(1:3), x1=runif(3), x2=rnorm(3)))

  to.keep <- as.integer(c(1,3) )
  tmp.s <- stUpdate(tmp, to.keep, type="spatial")

  checkEquals( tmp.s@t.id, to.keep)
  checkEquals( tmp.s@s.id, to.keep)
  checkTrue( is(tmp.s, testClassName))
  checkTrue( validObject(tmp.s))
}

test.stJoin <- function(){
  testClassName <- "stDataFrame"
  d1 <- new("stDataFrame", s.id=as.integer(1:3), t.id=as.integer(1:3),
             df=data.frame( s.id=as.integer(1:3), t.id=as.integer(1:3), x1=runif(3), x2=rnorm(3)))
  d2 <- new("stDataFrame", s.id=as.integer(8:10), t.id=as.integer(11:13),
             df=data.frame( s.id=as.integer(8:10), t.id=as.integer(11:13), x1=runif(3), x2=rnorm(3)))

  new.tid <- 1:3
  new.sid <- 1:5
  x.maps <- list(tid.map=1:3, sid.map=3:5)
  y.maps <- list(tid.map=1:3, sid.map=1:3)
  j <- stJoin(d1,d2, x.maps, y.maps, new.tid, new.sid)

  checkEquals(getTid(j), new.tid)
  checkEquals(getSid(j), new.sid)
  checkEquals( j@df$x1[1:3], d1@df$x1)
  checkEquals( j@df$x2[1:3], d1@df$x2)
  checkEquals( j@df$x1[4:6], d2@df$x1)
  checkEquals( j@df$x2[4:6], d2@df$x2)
  checkTrue( is(j, testClassName))
  checkTrue( validObject(j) )
}
