library(sp)
library(timeDate)
library(RCurl)     ## to get air explorer data
library(maps)      ## state maps
library(fda)       ## for plotting spatial points over time
data(stateMapEnv)


## distance matrix over space, time
setGeneric("stDist",   function(sp, type, ...)   {standardGeneric("stDist")} )

## to subset by space, time
setGeneric("stSubset", function(x, bounds, ...)  {standardGeneric("stSubset")} )

## to join two st objects.
setGeneric("stJoin", function(x, y, ...)  {standardGeneric("stJoin")} )

## to update the s.id and t.id in case of change in the other
## eg updating time may require an updating of space, and vice-versa
setGeneric("stUpdate", function(st, new.id, ...) {standardGeneric("stUpdate")} )

## to update the s.id and t.id in case of change in the other
## eg updating time may require an updating of space, and vice-versa
setGeneric("stApply", function(st, colname, format, fun, by.site) {standardGeneric("stApply")} )


## Given a SpatioTemporalDataFrame, retun the dense matrix for the particular
## dataframe colname, eg matrix where the rows are the time indices
## columns are the spatial locations,
## so m[i,j] is the observation for 'colname' at time i for location j
## where the colnames and rownames have the unique ids.
setGeneric("getTimeBySpaceMat", function(st, colname) {standardGeneric("getTimeBySpaceMat")} )

############################################################
##
## CONSTRUCTORS
##
setGeneric("stSpatialIrregularGrid", function(st, indices.cols, ...) {standardGeneric("stSpatialIrregularGrid")} )
############################################################

############################################################
##
## GETTERS & SETTERS
##
############################################################


############################################################
##
## GETTERS
##
setGeneric("getSid", function(x){ standardGeneric("getSid")})
setGeneric("getSpatialPoints", function(x){ standardGeneric("getSpatialPoints")})
setGeneric("getSpatialGrid", function(x){ standardGeneric("getSpatialGrid")})

setGeneric("getTid", function(x){ standardGeneric("getTid")})
##setGeneric("periodicity", function(x){ standardGeneric("periodicity")}) ## TBD, Fix
setGeneric("getTimedatestamps", function(x, ...){ standardGeneric("getTimedatestamps")})
setGeneric("getTimeFormat", function(x, ...){ standardGeneric("getTimeFormat")})
##setGeneric("getTimeByFormat", function(x, format){ standardGeneric("getTimeByFormat")}) ## DEPRECATED

setGeneric("getDataFrame", function(x,...){ standardGeneric("getDataFrame")})

setGeneric("getstTemporal", function(x){ standardGeneric("getstTemporal")})
setGeneric("getstSpatial", function(x){ standardGeneric("getstSpatial")})
setGeneric("getstDataFrame", function(x){ standardGeneric("getstDataFrame")})

##
##
############################################################

############################################################
##
## SETTERS
##
setGeneric("setSid<-", function(object, value){ standardGeneric("setSid<-")})
setGeneric("setSpatialPoints<-", function(object, value){ standardGeneric("setSpatialPoints<-")})
setGeneric("setSpatialGrid<-", function(object, value){ standardGeneric("setSpatialGrid<-")})

setGeneric("setTid<-", function(object,value){ standardGeneric("setTid<-")})
setGeneric("setTimedatestamps<-", function(object, value){ standardGeneric("setTimedatestamps<-")})
setGeneric("setTimeFormat<-", function(object, value){ standardGeneric("setTimeFormat<-")})

setGeneric("setDataFrame<-", function(object, value){ standardGeneric("setDataFrame<-")})
##
##
############################################################



