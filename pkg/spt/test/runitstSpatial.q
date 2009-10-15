
##.setUp <- function() {  ## called before each test case, see also .tearDown()
##  print(".setUp")
##}
checkTrue(require(methods))

test.stSpatialPoints <- function(){
  testClassName <- "stSpatialPoints"
  n <- 10# num points
  d <- 2 # dimension
  checkTrue(require("sp"))
  for (n in c(1,5,100)){
    for (d in c(2,4)){
      crd <- matrix(runif(n*d),n,d)
      bbox <- cbind( apply(crd,2,min), apply(crd,2,max) )
      colnames(bbox) <- c("min","max")
      this <- new(testClassName, s.id=as.integer(1:n), coords=crd, bbox=bbox, proj4string = CRS(as.character(NA)) )
      checkTrue( is(this, testClassName))
      checkTrue( validObject(this))
    }
  }
  
#  checkException(new("stSpatialPoints", s.id=as.numeric(1:n), coords=matrix(runif(n*d),n,d), bbox=NULL, proj4string = CRS(as.character(NA)) ))
  
}


test.stDist <- function(){

  testClassName <- "stSpatialPoints"

  ## 2d test
  n <- 10
  d <- 2

  load("teststspatial.Rdata")

  crd <- matrix(as.numeric(1:(n*d)),n,d)
  bbox <- cbind( apply(crd,2,min), apply(crd,2,max) )
  colnames(bbox) <- c("min","max")
  this <- new(testClassName, s.id=as.integer(1:n), coords=crd, bbox=bbox, proj4string = CRS(as.character(NA)) )
  
  checkEquals(stDist(this,"euclidean"), stdisteu2d)
  checkEquals(stDist(this,"earth"), stdistea2d)

  ## 3d test
  d <- 3
  crd <- matrix(as.numeric(1:(n*d)),n,d)
  bbox <- cbind( apply(crd,2,min), apply(crd,2,max) )
  colnames(bbox) <- c("min","max")
  this <- new(testClassName, s.id=as.integer(1:n), coords=crd, bbox=bbox, proj4string = CRS(as.character(NA)) )
  checkEquals(stDist(this,"euclidean"), stdisteu3d)
  checkEquals(stDist(this,"earth"), stdistea3d)
}


test.stUpdate <- function(){
  testClassName <- "stSpatialPoints"
  n <- 10; d <- 2
  crd <- matrix(as.numeric(1:(n*d)),n,d)
  bbox <- cbind( apply(crd,2,min), apply(crd,2,max) )
  colnames(bbox) <- c("min","max")
  tmp <- new(testClassName, s.id=as.integer(1:n), coords=crd, bbox=bbox, proj4string = CRS(as.character(NA)) )

  to.keep <- as.integer((1:10)[-(3:7)]) 
  tmp.up <- stUpdate(tmp, to.keep )
  checkEquals( tmp.up@s.id, to.keep)
  checkEquals( tmp.up@coords, crd[to.keep,])  
  checkTrue( is(tmp.up, testClassName))
  checkTrue( validObject(tmp.up))

}


test.stSubset <- function(){
  testClassName <- "stSpatialPoints"
    
  n <- 10; d <- 2  
  crd <- matrix(as.numeric(rep(1:n,d)),n,d)
  colnames(crd) <- paste("coords.x",1:2,sep="")
  bbox <- cbind( apply(crd,2,min), apply(crd,2,max) )
  colnames(bbox) <- c("min","max")
  tmp <- new(testClassName, s.id=as.integer(1:n), coords=crd, bbox=bbox, proj4string = CRS(as.character(NA)) )
  tmp.sub <- stSubset(tmp, rbind( c(2,2), c(7,7)))
  
  checkEquals( tmp.sub@s.id, 2:6)
  checkEquals( tmp.sub@coords, crd[2:6,])
  checkTrue( is(tmp.sub, testClassName))
  checkTrue( validObject(tmp.sub))

  pts <- SpatialPoints(cbind( long=c(-81,-80), lat=c(31,33) ) )
  tmp <- new(testClassName, s.id=as.integer(1:2), coords=pts@coords, bbox=pts@bbox, proj4string=pts@proj4string)
  tmp.sub <- stSubset(tmp, rbind( c(-80.5,-79), c(32,34)) )
  
  checkEquals( tmp.sub@s.id, 2)
  checkEquals( as.numeric(tmp.sub@coords), as.numeric(pts@coords[2,]))  
  checkTrue( is(tmp.sub, testClassName))
  checkTrue( validObject(tmp.sub))


  tmp.sub <- stSubset(tmp, 2.0 )
  checkEquals( tmp.sub@s.id, as.integer(2) )
  checkException( stSubset(tmp, 1.2 ) )
}

test.stJoin <- function(){
  testClassName <- "stSpatialPoints"
  pts <- SpatialPoints(cbind( long=c(-81,-80), lat=c(31,33) ) )
  sp.1 <- new(testClassName, s.id=as.integer(1:2), coords=pts@coords, bbox=pts@bbox, proj4string=pts@proj4string)
  pts <- SpatialPoints(cbind( long=c(-81,-82), lat=c(31,33) ) )
  sp.2 <- new(testClassName, s.id=as.integer(1:2), coords=pts@coords, bbox=pts@bbox, proj4string=pts@proj4string)
  
  j <- stJoin(sp.1,sp.2)
  true.jc <- cbind( long=c(-82:-80), lat=c(33,31,33))
  checkEquals(coordinates(j$stSpatialPoints), true.jc)  
  checkEquals(j$x.id.map, 2:3)
  checkEquals(j$y.id.map, 2:1)
  checkTrue( is(j$stSpatialPoints, testClassName))
  checkTrue( validObject(j$stSpatialPoints))
  
  pts <- SpatialPoints(cbind( long=c(-81,-82), lat=c(31,33), elev=c(100,300) ) )
  sp.3 <- new(testClassName, s.id=as.integer(1:2), coords=pts@coords, bbox=pts@bbox, proj4string=pts@proj4string)
  checkException( stJoin(sp.1, sp.3))
}
