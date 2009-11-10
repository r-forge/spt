
validstSpatialIrregularGridObject <- function(object) {
  l.n.sid <- length(object@s.id)
  l.n.coord <- nrow(coordinates(object@coords))
  if (l.n.sid != l.n.coord )
    paste("Unequal unique spatial id (s.id), unique coordinate counts: ", l.n.sid, ", ",l.n.coord, sep="")
  if ( !identical( class(object@s.id), "integer"))
    paste("The class of s.id must be integer, was given as '",class(object@s.id),"'",sep="")
  return(TRUE)
}
## assign the function as the validity method for the class
setValidity("stSpatialIrregularGrid", validstSpatialIrregularGridObject)

setMethod("show","stSpatialIrregularGrid",
          function(object)return( getDataFrame(object)) )

setMethod("summary","stSpatialIrregularGrid",
          function(object){cat("A collection of",length(object@s.id),"points, in a grid of size",paste(object@grid@cells.dim,collapse=" x "),".\n") } )

setMethod("getDataFrame", signature(x="stSpatialIrregularGrid"),
          function(x) return( data.frame(sid=x@s.id, coordinates(x) )) 
        )


############################################################
##
## Constructor from flat file.
##
############################################################

## Given a flat file/data frame with the following columns:
## indices [i, j]  2 cols... [x, y]
##   i=1,..,n_x; j=1,..,n_y
##   originally, this irregular grid is indexed with (1,1) in the *LOWER LEFT*
##   the final grid returned/stored has (1,1) in the *UPPER LEFT*, in matrix index format
## center [long, lat] 2 cols... [long, lat]
## dates 1 col...
## observations > 0 cols...

## Constructors
setMethod("stSpatialIrregularGrid", signature(st="SpatialPolygons", indices.cols="integer"),
          function(st, indices.cols){
            return( new("stSpatialIrregularGrid", polygons=st@polygons, plotOrder=st@plotOrder, bbox=st@bbox, proj4string=st@proj4string  ,s.id=indices.cols))
          })



setMethod("stSpatialIrregularGrid", signature(st="data.frame", indices.cols="integer"),
          function(st, indices.cols, center.cols){
            df <- st
            nr <- max(df[, indices.cols[1]]) # num rows in *orig* grid
            nc <- max(df[, indices.cols[2]]) # num cols in *orig* grid
            ## now create matrices for the long, lat of the *center* of each grid cell
            centers <- list()
            centers$long <- centers$lat <- matrix(NA, nrow=nr, ncol=nc)
            for (j in 1:nc){
              for (i in 1:nr){
                curr.row.df <- (j-1)*nr + i
                centers$long[i,j] <- df[curr.row.df, center.cols[1] ]
                centers$lat[i,j] <- df[curr.row.df, center.cols[2] ]
              }
            }
            ##   originally, this irregular grid is indexed with (1,1) in the *LOWER LEFT*
            ##   the final grid returned/stored has (1,1) in the *UPPER LEFT*, in matrix index format
            centers$lat <- centers$lat[nr:1,]
            centers$long <- centers$long[nr:1,]
            irregGrid <- list()
            ## inner.{lat,long} are the *INTERPOLATED* grid cell boundaries
            ##  eg they are missing the outer rows/columns of the original grid
            ## These are the *INTERPOLATED* grid cell boundaries from before, plus
            ## the *EXTRAPOLATE* grid cell boundaries (outer rows/columns)
            irregGrid$final.lat <- irregGrid$final.long <- matrix(NA, ncol=nc+1, nrow=nr+1)
            for (i in 1:(nr-1)){
              for (j in 1:(nc-1)){
                irregGrid$final.lat[i+1, j+1] <- mean( c( centers$lat[i,j], centers$lat[i,j+1], centers$lat[i+1,j], centers$lat[i+1,j+1]) )
                irregGrid$final.long[i+1, j+1] <- mean( c(centers$long[i,j], centers$long[i,j+1], centers$long[i+1,j], centers$long[i+1,j+1]))
              }
            }
            ## Use symmetry to find point (xhat, yhat) st.
            ## (x[2],y[2]) is the midpoint between
            ## (xhat, yhat) and (x[1], y[1])
            ## Order is counter clockwise so we don't miss next-to-corner grid cell boundaries (except for 1 in UR)
            plot(centers$long, centers$lat, pch=46, asp=1)
            points(irregGrid$final.long, irregGrid$final.lat, pch=46, col="green")
            map("state",add=T,interior=T)
            ## Extrapolate top row grid boundaries:
            for (j in 1:(nc-1)){  # because irregGrid$final.x[i+1, nc+1] doesn't exist
              i <- 1 # top row
              x <- c(irregGrid$final.long[i+1, j+1], centers$long[i,j])
              y <- c(irregGrid$final.lat[i+1, j+1], centers$lat[i,j])
              yhat <- 2*y[2]-y[1]
              xhat <- 2*x[2]-x[1]
              irregGrid$final.lat[i,j] <- yhat
              irregGrid$final.long[i,j] <- xhat
                                        #    points(xhat, yhat, col="red", pch=46)
              points(irregGrid$final.long[i,j], irregGrid$final.lat[i,j], col="red", pch=46)
            }
            ## Get the [1,nc] entry, but have to change the points we're using...
            x <- c(irregGrid$final.long[2, nc-1], centers$long[1,nc-1])
            y <- c(irregGrid$final.lat[2, nc-1], centers$lat[1,nc-1])
            yhat <- 2*y[2]-y[1]
            xhat <- 2*x[2]-x[1]
            irregGrid$final.lat[1,nc] <- yhat
            irregGrid$final.long[1,nc] <- xhat
            points(irregGrid$final.long[1,nc], irregGrid$final.lat[1,nc], col="orange", pch=46)
            ## Extrapolate left column grid boundaries: 
            for (i in 1:nr){
              j <- 1 # left col
              x <- c(irregGrid$final.long[i, j+1], centers$long[i,j])
              y <- c(irregGrid$final.lat[i, j+1], centers$lat[i,j])
              yhat <- 2*y[2]-y[1]
              xhat <- 2*x[2]-x[1]
              irregGrid$final.lat[i+1,j] <- yhat
              irregGrid$final.long[i+1,j] <- xhat
                                        #    points(xhat, yhat, col="blue", pch=46)
              points(irregGrid$final.long[i+1,j], irregGrid$final.lat[i+1,j], col="blue", pch=46)
            }
            ## Extrapolate bottom row grid boundaries: (col 1 done from above)
            for (j in 1:nc ){
              i <- nr # bottom row
              x <- c(irregGrid$final.long[i, j], centers$long[i,j])
              y <- c(irregGrid$final.lat[i, j], centers$lat[i,j])
              yhat <- 2*y[2]-y[1]
              xhat <- 2*x[2]-x[1]
              irregGrid$final.lat[i+1,j+1] <- yhat
              irregGrid$final.long[i+1,j+1] <- xhat
                                        #    points(xhat, yhat, col="red", pch=46)
              points(irregGrid$final.long[i+1,j+1], irregGrid$final.lat[i+1,j+1], col="red", pch=46)
            }
            ## Extrapolate right column grid boundaries: 
            for (i in nr:1 ){
              j <- nc # right col
              x <- c(irregGrid$final.long[i+1, j], centers$long[i,j])
              y <- c(irregGrid$final.lat[i+1, j], centers$lat[i,j])
              yhat <- 2*y[2]-y[1]
              xhat <- 2*x[2]-x[1]
              irregGrid$final.lat[i,j+1] <- yhat
              irregGrid$final.long[i,j+1] <- xhat
                                        #    points(xhat, yhat, col="blue", pch=46)
              points(irregGrid$final.long[i,j+1], irregGrid$final.lat[i,j+1], col="blue", pch=46)
            }
            ## Now we can make polygons for each grid cell...
            cnt <- 1
            poly.list <- list()
            ## FILL IN COLUMN MAJOR ORDER!!!  THIS IS THE STANDARD HERE!!!
            for (j in 1:nc) {  ## Just have to make sure that we have the coordinates in the right order
              for (i in 1:nr) {
                lats <- c( irregGrid$final.lat[i,j], irregGrid$final.lat[i+1,j], irregGrid$final.lat[i+1,j+1], irregGrid$final.lat[i,j+1], irregGrid$final.lat[i,j] )
                longs <- c( irregGrid$final.long[i,j], irregGrid$final.long[i+1,j], irregGrid$final.long[i+1,j+1], irregGrid$final.long[i,j+1], irregGrid$final.long[i,j] )
                poly.list[[cnt]] <- Polygons( list( Polygon(cbind( longs, lats)) ), paste("poly",cnt,sep="_"))
                cnt <- cnt+1
              }
            }
            spp <- SpatialPolygons(poly.list, 1:length(poly.list))
            return( stSpatialIrregularGrid(spp, as.integer(1:(nr*nc)))  )
          }
          )
  


#df <- ndf
#indices.cols <- 2:1
##center.cols <- 3:4
#time.col <- 5
#format <- "%Y-%m-%d"

SpatialIrregularGridTemporalDataFrame <- function(df, indices.cols, center.cols, time.col, format="%Y-%m-%d"){

  stsg <- stSpatialIrregularGrid(df, indices.cols, center.cols)
  
  tims <- as.character(df[,time.col])
  unique.times <- timeDate(unique(tims), format=format)
  t.id <- as.integer(1:length(unique.times))
  stt <- new("stTemporal", t.id=t.id, timedatestamps=unique.times)

  nr <- max(df[ , indices.cols[1] ])
  nc <- max(df[ , indices.cols[2] ])
  s.id <- as.integer(1:(nr*nc))
  ## Note that COLUMN MAJOR format is the default...
  new.df.sid.col <- as.integer( (df[ , indices.cols[2] ]-1) * nr + df[ , indices.cols[1] ] )
  new.df.tid.col <- match( as.character(timeDate(tims,format=format)), as.character(unique.times) )
  
  new.df <- data.frame( df[, -c(indices.cols, center.cols, time.col)], new.df.tid.col, new.df.sid.col)  
  names(new.df) <- c(names(df)[-c(indices.cols, center.cols, time.col)], "t.id","s.id")  
  stdf.new <- new( "stDataFrame", s.id=s.id, t.id=t.id, df=new.df )

  return( new("SpatialIrregularGridTemporalDataFrame", spatial=stsg, temporal=stt, data=stdf.new) )
#  plot(stsg,col=2,axes=TRUE,border=FALSE)  
#  map("state",add=T,interior=T)
  
}

SpatialIrregularGrid <- function(grid, grid.index, coords, bbox=matrix(NA), proj4string = CRS(as.character(NA)), s.id=as.integer(1:nrow(coords)) ){
  spsp <- SpatialIrregularGrid(coords, proj4string, bbox)
  return(new("stSpatialIrregularGrid", s.id=s.id, grid=grid, grid.index=grid.index, coords=as.matrix(spsp@coords), bbox=spsp@bbox, proj4string = spsp@proj4string))
}


stSpatialIrregularGrid <- function(grid, grid.index, coords, bbox=matrix(NA), proj4string = CRS(as.character(NA)), s.id=as.integer(1:nrow(coords)) ){
  spsp <- SpatialIrregularGrid(coords, proj4string, bbox)
  return(new("stSpatialIrregularGrid", s.id=s.id, grid=grid, grid.index=grid.index, coords=as.matrix(spsp@coords), bbox=spsp@bbox, proj4string = spsp@proj4string))
}

## Getters
setMethod("getSid", signature(x="stSpatialIrregularGrid"),
          function(x) return(x@s.id) )
#setMethod("getSpatialIrregularGrid", signature(x="stSpatialIrregularGrid"),
#          function(x)return( new("SpatialIrregularGrid", grid=x@grid, grid.index=x@grid.index, coords=x@coords, bbox=x@bbox, proj4string = x@proj4string)) )

## Setters
setReplaceMethod("setSid", signature("stSpatialIrregularGrid"),
          function(object,value){
            object@s.id <- value
            return(object)
          }
          )

#setReplaceMethod("setSpatialIrregularGrid", signature("stSpatialIrregularGrid"),
#          function(object,value){
#            validObject(value)
#            if ( exists(value@coords)){
##              if (nrow(value@coords)==1){
 #               object@coords <- t(as.matrix(value@coords))
 #             } else {
 #               object@coords <- as.matrix(value@coords)
 #             }
 #           } else {
 #             stop("Error: Trying to replace slot 'coords' with an object without such a slot name")
 #           }
 #           if ( exists(value@coords)){
 #             object@bbox <- value@bbox
 #           } else {
 #             stop("Error: Trying to replace slot 'bbox' with an object without such a slot name")
#            }
#            if ( exists(value@grid)){
#              object@grid <- value@grid
#            } else {
#              stop("Error: Trying to replace slot 'grid' with an object without such a slot name")
#            }
#            if ( exists(value@grid.index)){
#              object@grid.index <- value@grid.index
#            } else {
#              stop("Error: Trying to replace slot 'grid.index' with an object without such a slot name")
#            }
#            return(object)
#          }
#          )

## FIX TBD grid, grid.index need to be updated.
setMethod("stSubset", signature(x="stSpatialIrregularGrid", bounds="integer"),
          function(x,bounds){
            new.members <- which( x@s.id %in% bounds)
            x@s.id <- x@s.id[new.members]
            if (length(new.members)==1) {
              x@coords <- t(as.matrix(x@coords[new.members,]))
            } else {
              x@coords <- x@coords[new.members,]
              x@bbox <- cbind( apply( coordinates(x), 2, min,na.rm=TRUE),
                               apply( coordinates(x), 2, max,na.rm=TRUE) )
            }
            colnames(x@bbox) <- c("min","max")
            rownames(x@bbox) <- colnames( coordinates(x) )
            return( x )
          }
          )



## FIX TBD grid, grid.index need to be updated.
setMethod("stSubset", signature(x="stSpatialIrregularGrid", bounds="numeric"),
          function(x,bounds){
            if ( bounds-as.integer(bounds)==0 ){
              return( stSubset(x, as.integer(bounds)) )
            } else {
              stop("bounds must be an integer")
            }
          }
          )


## FIX TBD grid, grid.index need to be updated.
setMethod("stSubset", signature(x="stSpatialIrregularGrid", bounds="matrix"),
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
            x@bbox <- t(bounds)
            colnames(x@bbox) <- c("min","max")
            rownames(x@bbox) <- colnames( coordinates(x))
            if (length(new.members)==1) {
              return( stSpatialIrregularGrid(x@s.id[new.members], coords=t(as.matrix(coordinates(x)[new.members,])),
                                      bbox=x@bbox, proj4string=x@proj4string) )
            } else {
              return( stSpatialIrregularGrid(x@s.id[new.members], coords=coordinates(x)[new.members,],
                                      bbox=x@bbox, proj4string=x@proj4string))
            }
          }
          )

## FIX TBD grid, grid.index need to be updated.  Not guaranteed to work?
setMethod("stUpdate",c(st="stSpatialIrregularGrid", new.id="integer"),
          function(st, new.id){
            ## TBD make sure that new.ids are in old ids ??
            new.s.id <- new.id
            new.members <- which( st@s.id %in% new.s.id)
            ## TBD check new.members for NA?  
            ## TBD update bounding box
            if (length(new.members)==1){
              st@coords <- t(as.matrix(coordinates(st)[new.members,]))
              st@bbox <- cbind( as.numeric(coordinates(st)), as.numeric(coordinates(st)))
            } else {
              st@coords <- coordinates(st)[new.members,]
              st@bbox <- cbind( apply( coordinates(st), 2, min,na.rm=TRUE),
                               apply( coordinates(st), 2, max,na.rm=TRUE) )
            }
            colnames(st@bbox) <- c("min","max")
            rownames(st@bbox) <- colnames( coordinates(st))
            st@s.id <- new.s.id
            return( st )
          }
          )


setMethod("stDist", signature(sp="SpatialIrregularGrid", type="character"),
          ## Need to make sure that the rdist called is the one in fields library...
          function(sp, type, miles=TRUE) {
##            require(fields)
            if ( type == "euclidean" ){
              dst <- rdist(coordinates(sp))
              colnames(dst) <- getSid(sp)
              rownames(dst) <- getSid(sp)
              return(dst)
            } else if ( type =="earth" ) {
              ## coordinates needs to be in form [long, lat] for correct
              ## computation of earth distance...
              crds <- as.matrix(coordinates(sp))
              crd.names <- colnames(crds)
              indx <- pmatch( upper.case(crd.names[1]), upper.case(c("lat", "long", "lats", "longs", "latitude","longitude")))
              if (is.na(indx) | length(indx)==0 )
                stop("In order to calculate earth (great circle) distance, coordinate columns must have names 'lat' and 'long' ")
              if ( even(indx))
                ## if it's even, then we have long in column 1.
##                dst <- rdist.earth(crds, miles=miles)
                dst <- calc.dist( crds[,1], crds[,2], miles=miles)
              else
                ## have to pass long/lat in correct order.
##                dst <- rdist.earth(crds[,2:1], miles=miles)
                dst <- calc.dist( crds[,2], crds[,1], miles=miles)
              
              colnames(dst) <- getSid(sp)
              rownames(dst) <- getSid(sp)
              return(dst)
            } else {
              stop("distance type can only be 'euclidean' or 'earth' ")
            }
          })

## FIX TBD grid, grid.index need to be updated.
setMethod("stJoin", signature(x="stSpatialIrregularGrid", y="stSpatialIrregularGrid"),
          function(x,y){
            ## Fix, TBD check bbox, proj4str and more??
            x.c <- coordinates(x)
            y.c <- coordinates(y)
            if (ncol(x.c) != ncol(y.c))
              stop("You can only join stSpatialIrregularGrid objects which both have the same number of coordinates")
            sp <- SpatialIrregularGrid(unique( rbind(x.c, y.c)))
            new.sids <- 1:nrow( coordinates(sp))
            x.char <- apply(x.c, 1, paste, collapse=" ")
            y.char <- apply(y.c, 1, paste, collapse=" ")
            new.char <- apply(sp@coords[ order(sp@coords[,1]),], 1, paste, collapse=" ")
            x.id.map <- match(x.char, new.char)
            y.id.map <- match(y.char, new.char)            
            x@coords <- sp@coords[ order(sp@coords[,1]),]
            x@s.id <- new.sids
            x@bbox <- sp@bbox
            return(list(stSpatialIrregularGrid=x, x.id.map=x.id.map, y.id.map=y.id.map) )
          }
          )

