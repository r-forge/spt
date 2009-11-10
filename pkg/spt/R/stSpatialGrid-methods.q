
validstSpatialGridObject <- function(object) {
  l.n.sid <- length(object@s.id)
  l.n.coord <- nrow(coordinates(object@coords))
  if (l.n.sid != l.n.coord )
    paste("Unequal unique spatial id (s.id), unique coordinate counts: ", l.n.sid, ", ",l.n.coord, sep="")
  if ( !identical( class(object@s.id), "integer"))
    paste("The class of s.id must be integer, was given as '",class(object@s.id),"'",sep="")
  return(TRUE)
}
## assign the function as the validity method for the class
setValidity("stSpatialGrid", validstSpatialGridObject)

setMethod("show","stSpatialGrid",
          function(object)return( getDataFrame(object)) )

setMethod("summary","stSpatialGrid",
          function(object){cat("A collection of",length(object@s.id),"points, in a grid of size",paste(object@grid@cells.dim,collapse=" x "),".\n") } )

setMethod("getDataFrame", signature(x="stSpatialGrid"),
          function(x) return( data.frame(sid=x@s.id, coordinates(x) )) 
        )



## Getters
setMethod("getSid", signature(x="stSpatialGrid"),
          function(x) return(x@s.id) )
setMethod("getSpatialGrid", signature(x="stSpatialGrid"),
          function(x)return( new("SpatialGrid", grid=x@grid, grid.index=x@grid.index, coords=x@coords, bbox=x@bbox, proj4string = x@proj4string)) )

## Setters
setReplaceMethod("setSid", signature("stSpatialGrid"),
          function(object,value){
            object@s.id <- value
            return(object)
          }
          )


