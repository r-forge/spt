setClass("stTemporal", representation(timedatestamps="timeDate", t.id="integer"),
         prototype(timedatestamps=timeDate(NULL), t.id=integer() )  )
setClass("stSpatial", representation(s.id="integer","VIRTUAL") )

## note that order matters when it comes to looking for which method to call
## when inheritance is used.
setClass("stSpatialPoints", contains=c("SpatialPoints","stSpatial") )
setClass("stSpatialGrid", contains=c("SpatialGrid","stSpatial") )
setClass("stSpatialIrregularGrid", contains=c("SpatialPolygons","stSpatial") )

setClass("stDataFrame",
         representation(s.id="integer",
                        t.id="integer",
                        df="data.frame"))

setClass("SpatialPointsTemporalDataFrame",
         representation(spatial="stSpatialPoints",
                        temporal="stTemporal",
                        data="stDataFrame"))

setClass("SpatialIrregularGridTemporalDataFrame",
         representation(spatial="stSpatialIrregularGrid",
                        temporal="stTemporal",
                        data="stDataFrame"))

