

## 10/5/09
## I tried this without the prototype part and it
## caused problems (due to timeDate initialize) as
## far as I could tell...

setClass("stTemporal", representation(timedatestamps="timeDate", t.id="integer"),
         prototype(timedatestamps=timeDate(NULL), t.id=integer() )  )


setClass("stSpatial", representation(s.id="integer","VIRTUAL") )

## note that order matters when it comes to looking for which method to call
## when inheritance is used.
setClass("stSpatialPoints", contains=c("SpatialPoints","stSpatial") )
setClass("stDataFrame",
         representation(s.id="integer",
                        t.id="integer",
                        df="data.frame"))
setClass("SpatialTemporalDataFrame",
         representation(spatial="stSpatial",
                        temporal="stTemporal",
                        data="stDataFrame"))

## Why not contains = c(...)??
## better for inheritance?

## ?? Is this better?
##setClassUnion("SpatialTemporalDataFrame",c("stSpatial",
##                                          "stTemporal",
##                                          "stDataFrame"))
# Mikes first change
# Mikes first change
