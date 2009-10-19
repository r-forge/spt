###################################################
### chunk number 1:  eval=FALSE
###################################################
## install.packages("spt", repos="http://R-forge.R-project.org")


###################################################
### chunk number 2:  eval=FALSE
###################################################
## library(spt)


###################################################
### chunk number 3: 
###################################################
set.seed(13331)


###################################################
### chunk number 4: 
###################################################
tmp <- stTemporal(timeVals=c("2008-09-29","2009-01-14","2012-12-12"),format="%Y-%m-%d")
tmp
getTid(tmp)
getTimedatestamps(tmp)


###################################################
### chunk number 5: 
###################################################
n <- 10 # num points
d <- 2  # dimension
crd <- matrix(runif(n*d),n,d)
bbox <- cbind( apply(crd,2,min), apply(crd,2,max) )
colnames(bbox) <- c("min","max")
tmp <- new("stSpatialPoints", s.id=as.integer(1:n), coords=crd, bbox=bbox, proj4string = CRS(as.character(NA)) )
tmp
getSid(tmp)
getSpatialPoints(tmp)


###################################################
### chunk number 6: 
###################################################
tmp <- new("stDataFrame", s.id=as.integer(1:3), 
           t.id=as.integer(1:3), df=data.frame( s.id=as.integer(1:3), 
                                   t.id=as.integer(1:3), x1=runif(3), x2=rnorm(3)))
tmp


###################################################
### chunk number 7: 
###################################################
pollutant <- "Oz"
state <- c("SC","NC")
startDate <- "2007-01-01"
endDate <- "2007-01-30"
tmp <- getEPAAirExplorerData(pollutant, state, startDate, endDate)
tmp

plot(tmp,"Concentration",units="days")
