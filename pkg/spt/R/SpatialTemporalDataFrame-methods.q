
validSpatialTemporalDataFrameObject <- function(object) {
  ## TBD
  ## Want to make sure that only the s.id and t.id locations are included in the dataframe...
  validObject(object@spatial)
  validObject(object@temporal)
  validObject(object@data)

  if ( !identical(sort(object@spatial@s.id), sort(object@data@s.id) ) )
    cat("unique spatial identifiers 's.id' must match in spatial and data objects")
  if ( !identical(sort(object@temporal@t.id), sort(object@data@t.id) ) )
    cat("unique temporal identifiers 't.id' must match in temporal and data objects")
}
setValidity("SpatialTemporalDataFrame", validSpatialTemporalDataFrameObject)


## Getters, setters:
setMethod("getSid", signature(x="SpatialTemporalDataFrame"),
          function(x) return(x@spatial@s.id) )
setMethod("getSpatialPoints", signature(x="SpatialTemporalDataFrame"),
          function(x)return( new("SpatialPoints", coords=x@spatial@coords, bbox=x@spatial@bbox, proj4string = x@spatial@proj4string)) )
setMethod("getTid", signature(x="SpatialTemporalDataFrame"),
          function(x) return(x@temporal@t.id) )
setMethod("getTimedatestamps", signature(x="SpatialTemporalDataFrame"),
          function(x,format="%Y-%m-%d")return( getTimedatestamps(x@temporal,format)))
setMethod("getDataFrame", signature(x="SpatialTemporalDataFrame"),
          function(x)return( x@data@df) )
setMethod("getstSpatial",signature(x="SpatialTemporalDataFrame"),
          function(x)return( x@spatial) )
setMethod("getstTemporal",signature(x="SpatialTemporalDataFrame"),
          function(x)return( x@temporal) )
setMethod("getstDataFrame",signature(x="SpatialTemporalDataFrame"),
          function(x)return( x@data) )
##
setMethod("show", "SpatialTemporalDataFrame",
          function(object){#show(object@metadata)
                           show(object@data)}
          )
setMethod("getTimeBySpaceMat", signature(st="SpatialTemporalDataFrame", colname="character"),
          getTimeBySpaceMat <- function(st,colname){
            n.t <- length(getTid(st))
            n.s <- length(getSid(st))
            dens <- matrix(NA, nrow= n.t, ncol=n.s)
            rownames(dens) <- paste("tid",getTid(st),sep="_")
            colnames(dens) <- paste("sid",getSid(st),sep="_")
            id.pairs <- getDataFrame(st)[c("s.id","t.id")]
            for (i in 1:n.t){
              for (j in 1:n.s){
                tidMatches <- id.pairs$"t.id" == getTid(st)[i]
                sidMatches <- id.pairs$"s.id" == getSid(st)[j]
                if ( sum( tidMatches & sidMatches)==1){
                  dens[i,j] <- getDataFrame(st)[ tidMatches & sidMatches, colname]
                }
              }
            }
            ## Quickly make sure it's in order, swapping rows as necessary.
            class(dens)
            timeOrder <- order( getTimedatestamps(st,"%Y-%m-%d %H:%M"))
            dens <- dens[timeOrder,]
            return(dens)
          }
          )

setMethod("plot", signature(x="SpatialTemporalDataFrame", y="character"),
          function(x,y,units,plotType="fda",browse=FALSE) {
            if ( class(x@spatial) == "stSpatialPoints"){
              getGridCell <- function( pnt, xlims, ylims, x.len, y.len){
                ## return which entry of a gridded vector a value falls in
                ## pnt is the point whose grid cell you're looking for
                ## y lims are the min/max of the y dimension
                ## x lims are the min/max of the x dimension
                ## x.len is the number of bins in the x dimension
                ## y.len is the number of bins in the y dimension
                getBin <- function(val, min, stepsize) return(ceiling( (val-(min-1e-14))/stepsize ))
                return(c(i=1+y.len-getBin(pnt[2], ylims[1], diff(range(ylims))/(y.len-1)), j=getBin(pnt[1],xlims[1],diff(range(xlims))/(x.len-1))))
              }
              plotColorGrid <- function(x, y, x.len, y.len, returnbColor=FALSE){
                ## x,y: are vectors of length 2 
                ##  entries are the min/max of a grid spaced with {x,y}.len points
                ##
                ## x.len and y.len have to be integers (but it's not currently checked)
                ##
                ## setup a grid for the color matrix generation around the
                ## unit square for convenience.
                x.g <- seq(-1,1,len=x.len)
                y.g <- seq(-1,1,len=y.len)
                ## Calculate the distance and direction of each gridpoint relative to due east.
                ## Need to center them.
                dir <- matrix(0,x.len,y.len)
                dist <- matrix(0,x.len,y.len)
                for (i in 1:x.len){
                  for (j in 1:y.len){
                    dist[i,j] <- sqrt( x.g[i]^2 + y.g[j]^2)
                    dir[i,j] <- atan2( y.g[j], x.g[i]) *180 / pi + 180
                  }
                }
                bColor <- matrix(hcl( as.vector(dir), as.vector(dist)*100/max(dist), as.vector(dist)*100/max(dist) ), x.len,y.len)
                rm(dir,dist,x.g,y.g)
                x <- seq(min(x),max(x),len=x.len)
                y <- seq(min(y),max(y),len=y.len)
                gridwid <- diff(range(x))/x.len
                gridht <- diff(range(y))/y.len
                plot(c(0,0), xlim=c(min(x),max(x)), ylim=c(min(y),max(y)), type='n', ylab="Latitude",xlab="Longitude",asp=1)
                for (i in 1:x.len){
                  for (j in 1:y.len){
                    mat <- cbind( rep( c(x[i]+gridwid, x[i]-gridwid), each=2), c(y[j]-gridht, y[j]+gridht, y[j]+gridht, y[j]-gridht) )
                    polygon(mat, border=NA, col=bColor[i,j])
                  }
                }
                if (returnbColor)
                  return(bColor)
              }
              if (browse)
                browser()
              coords <- as.data.frame(getSpatialPoints(x))
              lat.breaks  <- 51
              long.breaks <- 51
              long.range <- range(coords[,1])
              lat.range <- range(coords[,2])
              locIndices <- t(apply( coords, 1, "getGridCell", range(coords[,1]), range(coords[,2]), long.breaks-1, lat.breaks-1))
              lay.mat <- matrix(2, nrow=6,ncol=4)
              lay.mat[5:6,2:3] <- 1
              lay.mat[5:6, c(1,4)] <- 0
              layout(lay.mat)
              bColor <- plotColorGrid(long.range, lat.range, long.breaks, lat.breaks, returnbColor=TRUE)
              points(coords, pch=19)
              map('state', add=TRUE, interior=TRUE)
              n.s <- nrow(coords)
              bColors <- rep(NA,n.s)
              for (i in 1:n.s) {
                loc <- getGridCell( as.matrix(coords[i,]), long.range, lat.range, long.breaks, lat.breaks)
                lo <- loc[1]
                la <- loc[2]
                bColors[i] <- bColor[lo, lat.breaks-la+1]
              }
              tByS <- getTimeBySpaceMat(x,y)
              t <- length(getTid(x))
              
              if (plotType=="lines"){
                
                x.locs <- sort(as.numeric(difftime( getTimedatestamps(x, "%Y-%m-%d %H:%M"), min(getTimedatestamps(x)), units=units)))
                sub <- paste("Starting from", as.character( min(getTimedatestamps(x,"%Y-%m-%d %H:%M"))))
                ylims <- range(tByS,na.rm=TRUE)
                i <- 1
                plot(x.locs, tByS[,i], type="l", col=bColors[i],xlab=paste("Time (",units,")",sep=""),ylab=y, sub=sub,ylim=ylims)
                if (n.s>1){
                  for (i in 2:n.s){
                    lines(x.locs, tByS[,i], col=bColors[i])
                  }
                }
              }
              
              if (plotType=="fda"){
                ## Have to remove columns with more than 0% missing data to get the plot to work right.
                tooManyMissing <- which(apply( apply(tByS, 2, is.na), 2, sum) > 0)
                tByS <- tByS[, -tooManyMissing ]
                bColors <- bColors[  -tooManyMissing ]
                if ( length(tooManyMissing) > 0) {
                  print(paste("There are",length(tooManyMissing),"stations with missing data"))
                  print(paste("which will not be shown.  That is",100*round(length(tooManyMissing)/n.s,3),"% of the sites"))
                }
                ## can sortt because tByS is in sorted order.
                fdargvals <- sort(as.numeric(difftime( getTimedatestamps(x,"%Y-%m-%d %H:%M"), min(getTimedatestamps(x)), units=units)))
                fb <- create.fourier.basis( c(0,t), ceiling(2*t/3))
                ofd <- Data2fd( argvals=fdargvals,  y=tByS, basis=fb )
                sub <- paste("Starting from", as.character( min(getTimedatestamps(x,"%Y-%m-%d %H:%M"))))
                plot(ofd, col=bColors, ylim=range(getDataFrame(x)[y]), lty=1, xlab=paste("Time (",units,")",sep=""),ylab=y, sub=sub)
              }
            }
          }
          )

## subset for temporal (time as timeDate)
setMethod("stSubset",c(x="SpatialTemporalDataFrame",bounds="timeDate"),
          function(x,bounds){
            ## 1) subset the temporalal part.
            new.stt <- stSubset(x@temporal, bounds)

            ## 2) Take the temporal part t.id, and then reduce the data frame
            new.stdf <- stUpdate(x@data, new.stt@t.id, type="temporal")

            ## 3) Take the new data frame and clean up temporal (if applicable).
            ##    to get rid of any orphan t.ids that no longer exist.
            new.sts <- stUpdate(x@spatial, new.stdf@s.id)
            x@spatial <- new.sts
            x@data <- new.stdf
            x@temporal <- new.stt
            gc()
            return(x)
          }
          )

## subset for temporal (time as char)
setMethod("stSubset",c(x="SpatialTemporalDataFrame",bounds="character"),
          function(x, bounds, format){
            return( stSubset(x, timeDate(bounds,format)))
          }
          )


## subset for ID (spatial or temporal).
setMethod("stSubset",c(x="SpatialTemporalDataFrame",bounds="integer"),
          function(x, bounds, type){
            if ( !exists("type")) stop("Using stUpdate with integer bounds requires updating either type 'temporal' or type 'spatial'; none provided ")
            if (type == "spatial"){
              new.sts <- stSubset(x@spatial, bounds)
              new.stdf <- stUpdate(x@data, new.sts@s.id, type="spatial")
              new.stt <- stUpdate(x@temporal, new.stdf@t.id)
              x@spatial <- new.sts
              x@data <- new.stdf
              x@temporal <- new.stt
              gc()
              return(x)
            } else if (type == "temporal"){
              new.stt <- stSubset(x@temporal, bounds)
              new.stdf <- stUpdate(x@data, new.stt@t.id, type="temporal")
              new.sts <- stUpdate(x@spatial, new.stdf@s.id)
              x@spatial <- new.sts
              x@data <- new.stdf
              x@temporal <- new.stt
              gc()
              return(x)
            } else {
              stop("Using stUpdate on integer bounds requires updating either type 'temporal' or type 'spatial' ")
            }
          }
          )


## subset for spatial
setMethod("stSubset",c(x="SpatialTemporalDataFrame",bounds="matrix"),
          function(x,bounds){
            ## 1) subset the spatial part.
            new.sts <- stSubset(x@spatial, bounds)

            ## 2) Take the spatial part s.id, and then reduce the data frame
            new.stdf <- stUpdate(x@data, new.sts@s.id, type="spatial")

            ## 3) Take the new data frame and clean up temporal (if applicable).
            ##    to get rid of any orphan t.ids that no longer exist.
            new.stt <- stUpdate(x@temporal, new.stdf@t.id)
            x@spatial <- new.sts
            x@data <- new.stdf
            x@temporal <- new.stt
            gc()
            return(x)
          }
          )



############################################################
##
## Constructor
##
SpatialTemporalDataFrame <- function( stdf, location.col, time.col, tformat="%Y-%m-%d"){
  ## Create a new stDF, given a full stdf, and the column numbers for location
  ## and time
  verbose <- FALSE
  require(sp)
  nrow <- nrow(stdf)
  ncol <- ncol(stdf)
  n.coord <- length(location.col)
  if ( any(location.col > ncol, time.col > ncol) ) {
    stop("indices for location and time columns must not be greater than the dataframe's ncol")
  }
  if (verbose) 
    pp("Extracting the locations and times from the data.frame...",Sys.time())
  locs <- stdf[,location.col]
  tims <- as.character(stdf[,time.col])
#  browser()
  s.id <- 1:nrow(unique(locs))  ## assumes locs is > 1!!! FIX!
  unique.locs <- unique(locs)
  if (verbose) 
    pp("Creating an stSpatialPoints object...",Sys.time())
  spsp <- SpatialPoints(unique(locs))
  sts <- new("stSpatialPoints", s.id=s.id, coords=spsp@coords, proj4string=spsp@proj4string, bbox=spsp@bbox)

  if (verbose) 
    pp("Creating an stTemporal object",Sys.time())
  unique.times <- timeDate(unique(tims), format=tformat)
  t.id <- 1:length(unique.times)
  stt <- new("stTemporal", t.id=t.id, timedatestamps=unique.times)

  ## construct stdf by removing the time and coordinate information.
  ## replace them with 2 new columns, s.id, t.id, and put in the corresponding
  ## id.
  ##
  ## match over unique.locs and unique.times

  ## ISSUE!
  ## match only seems to work on vectors...  so I coerce to character vector
  ## until somebody thinks of a better match for matrix rows...
  if (verbose) 
    pp("Determining unique IDs...",Sys.time())
  locs.as.char <- apply(locs,1,paste,collapse=" ")
  unique.locs.as.char <- apply(unique.locs,1,paste,collapse=" ")
  new.df.sid.col <- match( locs.as.char, unique.locs.as.char)
  new.df.tid.col <- match( as.character(timeDate(tims,format=tformat)), as.character(unique.times) )
  if (verbose) 
    pp("Creating an stDataFrame object",Sys.time())
  new.df <- data.frame( stdf[, -c(location.col, time.col)], new.df.tid.col, new.df.sid.col)  
  names(new.df) <- c(names(stdf)[-c(location.col, time.col)], "t.id","s.id")
  stdf.new <- new( "stDataFrame", s.id=s.id, t.id=t.id, df=new.df )
  if (verbose) 
    pp("Creating a SpatialTemporalDataFrame object",Sys.time())
  return( new("SpatialTemporalDataFrame", spatial=sts, temporal=stt, data=stdf.new) )
}

as.data.frame.SpatialTemporalDataFrame <- function(from) {
        df.new <- getDataFrame(from)
        ## now replace the s.id and t.id with the actual values.
        new.temporal.col <- getTimedatestamps(from)[ match( df.new$t.id, getTid(from) ) ]
        new.spatial.col <- getSpatialPoints(from)[ match( df.new$s.id, getSid(from) ), ]
        df.names <- names(df.new)
        df.names <- df.names[ !(df.names %in% c("t.id","s.id")) ]
        return( data.frame( df.new[df.names], temporal=new.temporal.col, spatial=new.spatial.col) )
}

setAs("SpatialTemporalDataFrame", "data.frame",
      function(from){
        df.new <- getDataFrame(from)
        ## now replace the s.id and t.id with the actual values.
        new.temporal.col <- getTimedatestamps(from)[ match( df.new$t.id, getTid(from) ) ]
        new.spatial.col <- getSpatialPoints(from)[ match( df.new$s.id, getSid(from) ), ]
        df.names <- names(df.new)
        df.names <- df.names[ !(df.names %in% c("t.id","s.id")) ]
        return( data.frame( df.new[df.names], temporal=new.temporal.col, spatial=new.spatial.col) )
      }
      )



setMethod("stDist",c(sp="SpatialTemporalDataFrame",type="character"),
          function(sp,type,units){
            if ( !exists("type")) stop("Using stUpdate on an stDataFrame requires updating either type 'temporal' or type 'spatial'; none provided ")
            if (type == "spatial"){
              return(stDist(sp@spatial,units))
            } else if (type == "temporal"){
              return(stDist(sp@temporal,units))
            } else {
              stop("Using stUpdate on an stDataFrame requires updating either type 'temporal' or type 'spatial' ")
            }
          })

setMethod("stApply",c(st="SpatialTemporalDataFrame", colname="character", format="character", fun="function", by.site="logical"),
          function(st, colname, format, fun, by.site){
            tds.formatted <- getTimedatestamps(st,format)
            tdsfm <- tds.formatted[match( st@data@df$"t.id", getTid(st) ) ]
            ## now, create a new df.
            if ( !by.site ){
              x <- data.frame( st@data@df[colname])
              return(gapply(x, which=1, FUN=fun, group=tdsfm) )
            } else {
              ## do gapply thing for each location.
              sid <- getSid(st)
              x <- data.frame( st@data@df[colname], sid=st@data@df["s.id"]) 
              #cbind(x, tdsfm)
              out <- list()
              for (i in 1:length(sid)) {
                x1 <- x[(x$s.id == sid[i]) ,]
               # x1
                st.grp <- tdsfm[(x$s.id == sid[i])]
                out[[i]] <- gapply(x1, which=1, FUN=mean, group=st.grp)

              }
              return(out)
            }
            

          }
          )

setMethod("stJoin", c(x="SpatialTemporalDataFrame", y="SpatialTemporalDataFrame"),
          function(x,y){
            if (identical(NA,x) & identical(NA,y))
              stop("Cannot join two missing datasets")
            if (identical(NA,x))
              return(y)
            if (identical(NA,y))
              return(x)
            y.map <- x.map <- list()
            ## 1) Join temporal parts
            stt.join <- stJoin(getstTemporal(x), getstTemporal(y))
            new.stt <- stt.join$stTemporal
            x.map$tid.map <- stt.join$x.id.map
            y.map$tid.map <- stt.join$y.id.map
            
            ## 2) Join spatial parts
            sts.join <- stJoin(getstSpatial(x), getstSpatial(y))
            new.sts <- sts.join$stSpatialPoints
            x.map$sid.map <- sts.join$x.id.map
            y.map$sid.map <- sts.join$y.id.map
            
            ## 3) Join data parts
            new.stdf <- stJoin( getstDataFrame(x), getstDataFrame(y), x.map, y.map, getTid(new.stt), getSid(new.sts) )

            ## overwrite and return x
            x@spatial <- new.sts
            x@temporal <- new.stt
            x@data <- new.stdf
            return(x)
          }
          )

##
############################################################
