
validstSpatialPointsObject <- function(object) {
  l.n.sid <- length(object@s.id)
  l.n.coord <- nrow(coordinates(object@coords))
  if (l.n.sid != l.n.coord )
    paste("Unequal unique spatial id (s.id), unique coordinate counts: ", l.n.sid, ", ",l.n.coord, sep="")
  if ( !identical( class(object@s.id), "integer"))
    paste("The class of s.id must be integer, was given as '",class(object@s.id),"'",sep="")
  return(TRUE)
}
## assign the function as the validity method for the class
setValidity("stSpatialPoints", validstSpatialPointsObject)
setMethod("show","stSpatialPoints",
          function(object){cat("A collection of",length(object@s.id),"points.\n") } )

## Constructor
stSpatialPoints <- function(coords, bbox=matrix(NA), proj4string = CRS(as.character(NA)), s.id=as.integer(1:nrow(coords)) ){
  spsp <- SpatialPoints(coords, proj4string, bbox)
  return(new("stSpatialPoints", s.id=s.id, coords=as.matrix(spsp@coords), bbox=spsp@bbox, proj4string = spsp@proj4string))
}

## Getters
setMethod("getSid", signature(x="stSpatialPoints"),
          function(x) return(x@s.id) )
setMethod("getSpatialPoints", signature(x="stSpatialPoints"),
          function(x)return( new("SpatialPoints", coords=x@coords, bbox=x@bbox, proj4string = x@proj4string)) )

## Setters
setReplaceMethod("setSid", signature("stSpatialPoints"),
          function(object,value){
            object@s.id <- value
            return(object)
          }
          )

setReplaceMethod("setSpatialPoints", signature("stSpatialPoints"),
          function(object,value){
            validObject(value)
            if ( exists(value@coords)){
              if (nrow(value@coords)==1){
                object@coords <- t(as.matrix(value@coords))
              } else {
                object@coords <- as.matrix(value@coords)
              }
            } else {
              stop("Error: Trying to replace slot 'coords' with an object without such a slot name")
            }
            if ( exists(value@coords)){
              object@bbox <- value@bbox
            } else {
              stop("Error: Trying to replace slot 'bbox' with an object without such a slot name")
            }
            if ( exists(value@coords)){
              object@proj4string <- value@proj4string
            } else {
              stop("Error: Trying to replace slot 'proj4string' with an object without such a slot name")
            }
            return(object)
          }
          )

setMethod("stSubset", signature(x="stSpatialPoints", bounds="integer"),
          function(x,bounds){
            new.members <- which( x@s.id %in% bounds)
            x@s.id <- x@s.id[new.members]
            if (length(new.members)==1) {
              x@coords <- t(as.matrix(x@coords[new.members,]))
            } else {
              x@coords <- x@coords[new.members,]
            }
            return( x )
          }
          )

setMethod("stSubset", signature(x="stSpatialPoints", bounds="numeric"),
          function(x,bounds){
            if ( bounds-as.integer(bounds)==0 ){
              return( stSubset(x, as.integer(bounds)) )
            } else {
              stop("bounds must be an integer")
            }
          }
          )


setMethod("stSubset", signature(x="stSpatialPoints", bounds="matrix"),
          function(x, bounds){
            ## Check: make sure min/max have same number of coordinates as ncol(coordinates(x))
            inside <- function(point,bounds){
              ## bounds is a matrix with 2 rows.  1-d example [ 0, 1]'
              ## 2-d example [ 0 0, 1 1 ]
              ## 3-d example [ 0 0 0, 1 1 1 ]
              ## returns a boolean T/F if the point is located in the bounds.
              if (ncol(bounds) != length(point) ) stop("wrong input to 'inside'")
              return( all(point >= bounds[1,]) & all(point < bounds[2,])   )
            }
            new.members <- which( apply(coordinates(x), 1, inside, bounds))
            ## Fix adjust bbox to be correct?
            ## Fix: figure out the numeric/matrix issue...
            if (length(new.members)==1) {
              return( stSpatialPoints(x@s.id[new.members], coords=t(as.matrix(coordinates(x)[new.members,])),
                                      bbox=x@bbox, proj4string=x@proj4string) )
            } else {
              return( stSpatialPoints(x@s.id[new.members], coords=coordinates(x)[new.members,],
                                      bbox=x@bbox, proj4string=x@proj4string))
            }
          }
          )


setMethod("stUpdate",c(st="stSpatialPoints", new.id="integer"),
          function(st, new.id){
            ## TBD make sure that new.ids are in old ids ??
            new.s.id <- new.id
            new.members <- which( st@s.id %in% new.s.id)
            ## TBD check new.members for NA?  
            ## TBD update bounding box
            if (length(new.members)==1){
              st@coords <- t(as.matrix(coordinates(st)[new.members,]))
            } else {
              st@coords <- coordinates(st)[new.members,]
            }
            st@s.id <- new.s.id
            gc()
            
            return( st )
          }
          )


setMethod("stDist", signature(sp="SpatialPoints", type="character"),
          ## Need to make sure that the rdist called is the one in fields library...
          function(sp, type) {
            require(fields)
            if ( type == "euclidean" ){
              rdist(coordinates(sp))
            } else if ( type =="earth" ) {
              rdist.earth(coordinates(sp))
            } else {
              stop("distance type can only be 'euclidean' or 'earth' ")
            }
          })

setMethod("stJoin", signature(x="stSpatialPoints", y="stSpatialPoints"),
          function(x,y){
            ## Fix, TBD check bbox, proj4str and more??
            x.c <- coordinates(x)
            y.c <- coordinates(y)
            if (ncol(x.c) != ncol(y.c))
              stop("You can only join stSpatialPoints objects which both have the same number of coordinates")
            sp <- SpatialPoints(unique( rbind(x.c, y.c)))
            new.sids <- 1:nrow( coordinates(sp))
            x.char <- apply(x.c, 1, paste, collapse=" ")
            y.char <- apply(y.c, 1, paste, collapse=" ")
            new.char <- apply(sp@coords[ order(sp@coords[,1]),], 1, paste, collapse=" ")
            x.id.map <- match(x.char, new.char)
            y.id.map <- match(y.char, new.char)            
            x@coords <- sp@coords[ order(sp@coords[,1]),]
            x@s.id <- new.sids
            x@bbox <- sp@bbox
            return(list(stSpatialPoints=x, x.id.map=x.id.map, y.id.map=y.id.map) )
          }
          )

## 10/6; now it should work using inheritance...
#setMethod("plot", signature(x="stSpatialPoints", y="missing"),
#          function(x) { plot(x@coords) } )
#selectMethod("plot", signature(x="stSpatialPoints", y="missing"))
## Doesn't work; don't know why? (order of contains in class def.
##setMethod("plot", signature(x="stSpatialPoints", y="missing"),
##          function(x) { useNextMethod() } )
