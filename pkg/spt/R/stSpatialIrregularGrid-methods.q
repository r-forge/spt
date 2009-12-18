
validstSpatialIrregularGridObject <- function(object) {
  l.n.sid <- length(object@s.id)
  l.n.coord <- length(object@polygons)
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
          function(object){
            cat("Spatial: A collection of",length(object@s.id),"points, in a grid.\n")
          } )

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

setMethod("plot", signature(x="stSpatialIrregularGrid", y="ANY"),
          function(x, y, units, plotLegend=TRUE, browse=FALSE) {
            ## given 

            
          }
          )

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
#            centers$lat <- centers$lat[nr:1,]
#            centers$long <- centers$long[nr:1,]
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
#            plot(centers$long, centers$lat, pch=46, asp=1)
#            points(irregGrid$final.long, irregGrid$final.lat, pch=46, col="green")
#            map("state",add=T,interior=T)
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
#              points(irregGrid$final.long[i,j], irregGrid$final.lat[i,j], col="red", pch=46)
            }
            ## Get the [1,nc] entry, but have to change the points we're using...
            x <- c(irregGrid$final.long[2, nc-1], centers$long[1,nc-1])
            y <- c(irregGrid$final.lat[2, nc-1], centers$lat[1,nc-1])
            yhat <- 2*y[2]-y[1]
            xhat <- 2*x[2]-x[1]
            irregGrid$final.lat[1,nc] <- yhat
            irregGrid$final.long[1,nc] <- xhat
#            points(irregGrid$final.long[1,nc], irregGrid$final.lat[1,nc], col="orange", pch=46)
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
#              points(irregGrid$final.long[i+1,j], irregGrid$final.lat[i+1,j], col="blue", pch=46)
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
#              points(irregGrid$final.long[i+1,j+1], irregGrid$final.lat[i+1,j+1], col="red", pch=46)
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
#              points(irregGrid$final.long[i,j+1], irregGrid$final.lat[i,j+1], col="blue", pch=46)
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
            spp <- SpatialPolygons(poly.list, 1:(nr*nc) )
            tmp <- new("stSpatialIrregularGrid", polygons=spp@polygons, plotOrder=spp@plotOrder, bbox=spp@bbox, proj4string=spp@proj4string  ,s.id=as.integer(1:(nr*nc)))
            return( stSpatialIrregularGrid(spp, as.integer(1:(nr*nc)))  )
          }
          )


## Getters
setMethod("getSid", signature(x="stSpatialIrregularGrid"),
          function(x) return( x@s.id ) )
## Setters
setReplaceMethod("setSid", signature("stSpatialIrregularGrid"),
          function(object,value){
            object@s.id <- value
            return(object)
          }
          )

