

############################################################
############################################################
##
## SPATIALGRIDTEMPORALDATAFRAME follows;
##
############################################################
############################################################
setMethod("getTid", signature(x="SpatialGridTemporalDataFrame"),
          function(x) return( getTid(x@temporal) )        )

#setMethod("getTimedatestamps", signature(x="SpatialGridTemporalDataFrame"),
#          function(x, format=getTimeFormat(x)) return( getTimedatestamps(x@temporal, format=format) )  )

setMethod("getTimeFormat", signature(x="SpatialGridTemporalDataFrame"),
          function(x) return( getTimeFormat(x@temporal) )        )
setMethod("getSid", signature(x="SpatialGridTemporalDataFrame"),
          function(x) return( getSid(x@spatial))        )
setMethod("getDataFrame", signature(x="SpatialGridTemporalDataFrame"),
          function(x)return( x@data@df) )
setMethod("getstSpatial",signature(x="SpatialGridTemporalDataFrame"),
          function(x)return( x@spatial) )
setMethod("getstTemporal",signature(x="SpatialGridTemporalDataFrame"),
          function(x)return( x@temporal) )
setMethod("getstDataFrame",signature(x="SpatialGridTemporalDataFrame"),
          function(x)return( x@data) )

setMethod("summary","SpatialGridTemporalDataFrame",
          function(object){
            cat("\nClass: SpatialGridTemporalDataFrame\n")
            summary(object@spatial)
            summary(object@temporal)
            }
          )

setMethod("getTimeBySpaceMat", signature(st="SpatialGridTemporalDataFrame", colname="character"),
          function(st,colname){
            n.t <- length(getTid(st))
            n.s <- length(getSid(st))
            curr.df <- getDataFrame(st@data)
            if( nrow(curr.df) != (n.t*n.s))
              stop("getTimeBySpaceMat requires that there be no missing values")
            dens <- matrix( curr.df[ (order(curr.df$t.id, curr.df$s.id)), colname], nrow= n.t, ncol=n.s, byrow=TRUE)
            ## equivalently, fill it columnwise (but faster since it's already sorted by time then s.id)
            ##dens <- matrix( curr.df[ (order(curr.df$s.id, curr.df$t.id)), colname], nrow= n.t, ncol=n.s, byrow=FALSE)
            rownames(dens) <- paste("tid",getTid(st),sep="_")
            colnames(dens) <- paste("sid",getSid(st),sep="_")
            ## Quickly make sure it's in order, swapping rows as necessary.
            class(dens)
            timeOrder <- order( getTimedatestamps(st@temporal,"%Y-%m-%d %H:%M"))
            dens <- dens[timeOrder,]
            return(dens)
          }
          )



SpatialGridTemporalDataFrame <- function(df, indices.cols, center.cols, time.col, format="%Y-%m-%d"){
  stsg <- stSpatialGrid(df, indices.cols, center.cols)
  
  tims <- as.character(df[,time.col])
  unique.times <- timeDate(unique(tims), format=format)
  t.id <- as.integer(1:length(unique.times))
  stt <- new("stTemporal", t.id=t.id, timedatestamps=unique.times, timeFormat=format)

  nr <- max(df[ , indices.cols[1] ])
  nc <- max(df[ , indices.cols[2] ])
  s.id <- as.integer(1:(nr*nc))
  ## Note that COLUMN MAJOR format is the default for the S.IDs...
  new.df.sid.col <- as.integer( (df[ , indices.cols[2] ]-1) * nr + df[ , indices.cols[1] ] )
  new.df.tid.col <- match( as.character(timeDate(tims,format=format)), as.character(unique.times) )
  
  new.df <- data.frame( df[, -c(indices.cols, center.cols, time.col)], new.df.tid.col, new.df.sid.col)  
  names(new.df) <- c(names(df)[-c(indices.cols, center.cols, time.col)], "t.id","s.id")  
  stdf.new <- new( "stDataFrame", s.id=s.id, t.id=t.id, df=new.df )

  return( new("SpatialGridTemporalDataFrame", spatial=stsg, temporal=stt, data=stdf.new) )
}



############################################################
############################################################
##
## SPATIALIRREGULARGRIDTEMPORALDATAFRAME follows;
##
############################################################
############################################################
setMethod("getTid", signature(x="SpatialIrregularGridTemporalDataFrame"),
          function(x) return( getTid(x@temporal) )        )

setMethod("getTimedatestamps", signature(x="SpatialIrregularGridTemporalDataFrame", y="missing"),
          function(x) return( getTimedatestamps(x@temporal, getTimeFormat(x) ) )        )

setMethod("getTimedatestamps", signature(x="SpatialIrregularGridTemporalDataFrame", y="character"),
          function(x, y) return( getTimedatestamps(x@temporal, y) )        )

setMethod("getTimedatestamps", signature(x="SpatialIrregularGridTemporalDataFrame", y="numeric"),
          function(x, y, format=getTimeFormat(x)) return( getTimedatestamps(x@temporal, y, format=format) )        )

setMethod("getTimedatestamps", signature(x="SpatialIrregularGridTemporalDataFrame", y="integer"),
          function(x, y, format=getTimeFormat(x)) return( getTimedatestamps(x@temporal, y, format=format) )        )



setMethod("getTimeFormat", signature(x="SpatialIrregularGridTemporalDataFrame"),
          function(x) return( getTimeFormat(x@temporal) )        )
setMethod("getSid", signature(x="SpatialIrregularGridTemporalDataFrame"),
          function(x) return( getSid(x@spatial))        )
setMethod("getDataFrame", signature(x="SpatialIrregularGridTemporalDataFrame"),
          function(x)return( x@data@df) )
setMethod("getstSpatial",signature(x="SpatialIrregularGridTemporalDataFrame"),
          function(x)return( x@spatial) )
setMethod("getstTemporal",signature(x="SpatialIrregularGridTemporalDataFrame"),
          function(x)return( x@temporal) )
setMethod("getstDataFrame",signature(x="SpatialIrregularGridTemporalDataFrame"),
          function(x)return( x@data) )

setMethod("summary","SpatialIrregularGridTemporalDataFrame",
          function(object){
            cat("\nClass: SpatialIrregularGridTemporalDataFrame\n")
            summary(object@spatial)
            summary(object@temporal)
            }
          )

setMethod("getTimeBySpaceMat", signature(st="SpatialIrregularGridTemporalDataFrame", colname="character"),
          function(st,colname){
            n.t <- length(getTid(st))
            n.s <- length(getSid(st))
            curr.df <- getDataFrame(st@data)
            if( nrow(curr.df) != (n.t*n.s))
              stop("getTimeBySpaceMat requires that there be no missing values")
            dens <- matrix( curr.df[ (order(curr.df$t.id, curr.df$s.id)), colname], nrow= n.t, ncol=n.s, byrow=TRUE)
            ## equivalently, fill it columnwise (but faster since it's already sorted by time then s.id)
            ##dens <- matrix( curr.df[ (order(curr.df$s.id, curr.df$t.id)), colname], nrow= n.t, ncol=n.s, byrow=FALSE)
            rownames(dens) <- paste("tid",getTid(st),sep="_")
            colnames(dens) <- paste("sid",getSid(st),sep="_")
            ## Quickly make sure it's in order, swapping rows as necessary.
            class(dens)
            timeOrder <- order( getTimedatestamps(st@temporal,"%Y-%m-%d %H:%M"))
            dens <- dens[timeOrder,]
            return(dens)
          }
          )



SpatialIrregularGridTemporalDataFrame <- function(df, indices.cols, center.cols, time.col, format="%Y-%m-%d"){
  stsg <- stSpatialIrregularGrid(df, indices.cols, center.cols)  
  tims <- as.character(df[,time.col])
  unique.times <- timeDate(unique(tims), format=format)
  t.id <- as.integer(1:length(unique.times))
  stt <- new("stTemporal", t.id=t.id, timedatestamps=unique.times, timeFormat=format)

  nr <- max(df[ , indices.cols[1] ])
  nc <- max(df[ , indices.cols[2] ])
  s.id <- as.integer(1:(nr*nc))
  ## Note that COLUMN MAJOR format is the default for the S.IDs...
  new.df.sid.col <- as.integer( (df[ , indices.cols[2] ]-1) * nr + df[ , indices.cols[1] ] )
  new.df.tid.col <- match( as.character(timeDate(tims,format=format)), as.character(unique.times) )
  
  new.df <- data.frame( df[, -c(indices.cols, center.cols, time.col)], new.df.tid.col, new.df.sid.col)  
  names(new.df) <- c(names(df)[-c(indices.cols, center.cols, time.col)], "t.id","s.id")  
  stdf.new <- new( "stDataFrame", s.id=s.id, t.id=t.id, df=new.df )

  return( new("SpatialIrregularGridTemporalDataFrame", spatial=stsg, temporal=stt, data=stdf.new) )
}


setMethod("plot", signature(x="SpatialIrregularGridTemporalDataFrame", y="character"),
          function(x,y,units,mov.name="tmp",browse=FALSE) {
            ## x is the object
            ## y is the column name to plot (eg O3 or observed_o3 or NOX or...)
            ## units is...
            require(animation)
            
            ## Assign colors.
            plotOneDay <- function(spsp, curr.time.df, curr.df, curr.tid, obs.range, color.matlab){
              getColors <- function( vals, range, color.map){
                getBin <- function(val, min, stepsize) ifelse( val==min, return(1), return(ceiling( (val-(min))/stepsize) ))
                n.cm <- length(color.map)
                stepsize <- diff(range)/n.cm
                bins <- sapply(vals, getBin, range[1], stepsize)
                colors <- color.map[ bins]
                return(colors)
              }            
              plotSpatial <- function(spsp, curr.time.df, curr.df, curr.tid, obs.range, color.matlab){
                ## top plot (spatial)
                curr.cols <- getColors( curr.df[ (curr.df$t.id==curr.tid) , y ] , obs.range, color.matlab)
                curr.time <- format(curr.time.df[curr.tid, 2], format=timeFormat(spsp))
                titl <- paste("Observations:",y," At Time:",curr.time)
                plot(spsp, col= curr.cols, axes=TRUE, border=FALSE,main=titl,xlab="long",ylab="lat")
                title(main=titl)
                map("state",add=T,interior=T)
              }
              plotLegend <- function(obs.range, color.matlab, labs=10){
                ## top plot (legend on side for spatial)
                leg.mat <- matrix(1:length(color.matlab), ncol = 1)
                image(t(leg.mat), axes = FALSE, col = color.matlab)
                mylabel <- as.character(round(seq( obs.range[1], obs.range[2], length=labs ),1))
                axis(side = 2, at = seq(from = 0 - 1/labs/2, to = 1 + 1/labs/2,length = labs), labels = mylabel)
              }
              plotSpaghetti <- function(time.tick, min.time, units=units){
                ## bottom plot (temporal), FDA, aka spaghetti plot
                tByS <- getTimeBySpaceMat(x, y)
                t <- length(getTid(x))
                n.s <- length(getSid(x))
                x.locs <- sort(as.numeric(difftime( getTimedatestamps(x@temporal, "%Y-%m-%d %H:%M"), min(getTimedatestamps(x@temporal)), units=units)))
                sub <- paste("Starting from", as.character( min(getTimedatestamps(x@temporal,"%Y-%m-%d %H:%M"))))
                ylims <- range(tByS,na.rm=TRUE)
                i <- 1
                curr.time <- format(curr.time.df[curr.tid, 2], format="%Y-%m-%d")
                titl <- paste("Observations:",y," At Time:",curr.time)
                plot(x.locs, tByS[,i], type="l", col="grey", lty=3, xlab=paste("Time (",units,")",sep=""),ylab=y, sub=sub,ylim=ylims,main=titl)
                if (n.s>1){
                  for (i in 2:n.s){
                    lines(x.locs, tByS[,i], col="gray",lty=3)
                  }
                }
                abline(v=time.tick,lwd=3)
              }

              lay.mat <- matrix(1,3,3)  # plot spatial part
              lay.mat[,3] <- 2          # plot spatial legend
              lay.mat[3,] <- 3          # plot temporal part
              layout(lay.mat)
              ## top plot (spatial)
##              pp("starting plotSpatial..")
              plotSpatial(spsp, curr.time.df, curr.df, curr.tid, obs.range, color.matlab)
              
              ## top plot (legend on side for spatial)
##              pp("starting plotLegend..")
              plotLegend(obs.range, color.matlab)

              ## bottom plot (temporal), FDA, aka spaghetti plot
              curr.tim.row <- which( curr.time.df[,1]==curr.tid)
              min.time <- min(curr.time.df[,2])
              time.tick <- as.numeric( difftime( curr.time.df[curr.tim.row,2], min.time, units=units))
##              pp("starting plotSpaghetti..")
              plotSpaghetti(time.tick, min.time, units=units)
            }
            
            spsp <- x@spatial
            curr.df <- getDataFrame(x@data)
            obs.range <- range( curr.df[,y])
            obs.range[2] <- quantile( curr.df[,y], .995)
            sptim <- x@temporal
            curr.time.df <- getDataFrame(sptim, format, time.type="timeDate")
            if (browse)
              browser()
            #curr.tid <- 348
            #plotOneDay(spsp, curr.time.df, curr.df, curr.tid, obs.range, color.matlab)            
            
            sptMovie.ani <- function(...){
              for (curr.tid in getTid(x)){
                plotOneDay(spsp, curr.time.df, curr.df, curr.tid, obs.range, color.matlab)            
                Sys.sleep(ani.options("interval"))
              }
            }
            ## nmax is number of iterations,
            ## interval is time length we pause at each step.
#            saveSWF(sptMovie.ani(), interval = 1, swfname = "movie.swf", dev="pdf")
            oopt = ani.options(interval = 0.1, nmax = length(getTid(x)) )
            mov.dir <- paste(getwd(),"TmpMovie3",sep="/")
            dir.create( mov.dir)
            saveMovie(expr=sptMovie.ani(), dev = jpeg, height=1000, width=1000, outdir=mov.dir, movietype="gif", moviename=mov.name)
          }
          )


############################################################
############################################################
##
## SPATIALTEMPORALDATAFRAME follows;
## will soon be renamed SpatialPointsTemporalDataFrame
##
############################################################
############################################################

validSpatialPointsTemporalDataFrameObject <- function(object) {
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
setValidity("SpatialPointsTemporalDataFrame", validSpatialPointsTemporalDataFrameObject)


## Getters, setters:
setMethod("getSid", signature(x="SpatialPointsTemporalDataFrame"),
          function(x) return(x@spatial@s.id) )
setMethod("getSpatialPoints", signature(x="SpatialPointsTemporalDataFrame"),
          function(x)return( new("SpatialPoints", coords=x@spatial@coords, bbox=x@spatial@bbox, proj4string = x@spatial@proj4string)) )
setMethod("getTid", signature(x="SpatialPointsTemporalDataFrame"),
          function(x) return(x@temporal@t.id) )

setMethod("getTimedatestamps", signature(x="SpatialPointsTemporalDataFrame",y="missing"),
          function(x )return( getTimedatestamps(x@temporal, getTimeFormat(x) )))

setMethod("getTimedatestamps", signature(x="SpatialPointsTemporalDataFrame",y="character"),
          function(x, y )return( getTimedatestamps(x@temporal, y)))

setMethod("getTimedatestamps", signature(x="SpatialPointsTemporalDataFrame",y="integer"),
          function(x, y, format=getTimeFormat(x) )return( getTimedatestamps(x@temporal, y, format=format)))

setMethod("getTimedatestamps", signature(x="SpatialPointsTemporalDataFrame",y="numeric"),
          function(x, y, format=getTimeFormat(x) )return( getTimedatestamps(x@temporal, y, format=format)))

setMethod("getTimeFormat", signature(x="SpatialPointsTemporalDataFrame"),
          function(x)return( getTimeFormat(x@temporal)))
setMethod("getDataFrame", signature(x="SpatialPointsTemporalDataFrame"),
          function(x)return( x@data@df) )
setMethod("getstSpatial",signature(x="SpatialPointsTemporalDataFrame"),
          function(x)return( x@spatial) )
setMethod("getstTemporal",signature(x="SpatialPointsTemporalDataFrame"),
          function(x)return( x@temporal) )
setMethod("getstDataFrame",signature(x="SpatialPointsTemporalDataFrame"),
          function(x)return( x@data) )
##
setMethod("show", "SpatialPointsTemporalDataFrame",
          function(object){#show(object@metadata)
                           show(object@data)}
          )

setMethod("summary","SpatialPointsTemporalDataFrame",
          function(object){
            cat("\nClass: SpatialPointsTemporalDataFrame\n")
            summary(object@spatial)
            summary(object@temporal)
            n.miss <- sum( is.na(object@data@df))
            n.vals <- ( ncol(object@data@df)-2) * nrow(object@data@df)
            cat("\nMissingness: Additionally out of", (ncol(object@data@df)-2), "data columns,\n")
            cat(n.miss, "of", n.vals, "values, or about", round(n.miss/n.vals,3)*100, "% are missing\n")
          }
          )

setMethod("getTimeBySpaceMat", signature(st="SpatialPointsTemporalDataFrame", colname="character"),
          function(st,colname){
            n.t <- length(getTid(st))
            n.s <- length(getSid(st))
            dens <- t(reshape( getDataFrame(st)[c("s.id","t.id",colname)],
                              timevar="t.id", v.names=colname, idvar="s.id", direction="wide"))[2:(n.t+1),]
            tids <- unique( getDataFrame(st)$"t.id" )
            rownames(dens) <- paste("tid", tids,sep="_")
            colnames(dens) <- paste("sid", unique( getDataFrame(st)$"s.id"), sep="_" )
            ## Quickly make sure it's in order, swapping rows as necessary.
            timeOrder <- order( getTimedatestamps(st, tids,"%Y-%m-%d %H:%M"))
            dens <- dens[timeOrder,]
            return(dens)
          }
          )

setMethod("plot", signature(x="SpatialPointsTemporalDataFrame", y="character"),
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
setMethod("stSubset",c(x="SpatialPointsTemporalDataFrame",bounds="timeDate"),
          function(x,bounds){
            ## 1) subset the temporalal part.
            new.stt <- stSubset(x@temporal, bounds)
            ## TBD/Fix
            ## If null, return NA?

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
setMethod("stSubset",c(x="SpatialPointsTemporalDataFrame",bounds="character"),
          function(x, bounds, format){
            return( stSubset(x, timeDate(bounds,format)))
          }
          )


## subset for ID (spatial or temporal).
setMethod("stSubset",c(x="SpatialPointsTemporalDataFrame",bounds="integer"),
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
setMethod("stSubset",c(x="SpatialPointsTemporalDataFrame",bounds="matrix"),
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
SpatialPointsTemporalDataFrame <- function( stdf, location.col, time.col, format="%Y-%m-%d"){
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
  unique.times <- timeDate(unique(tims), format=format)
  t.id <- 1:length(unique.times)
  stt <- new("stTemporal", t.id=t.id, timedatestamps=unique.times, timeFormat=format)

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
  new.df.tid.col <- match( as.character(timeDate(tims,format=format)), as.character(unique.times) )
  if (verbose) 
    pp("Creating an stDataFrame object",Sys.time())
  new.df <- data.frame( stdf[, -c(location.col, time.col)], new.df.tid.col, new.df.sid.col)  
  names(new.df) <- c(names(stdf)[-c(location.col, time.col)], "t.id","s.id")
  stdf.new <- new( "stDataFrame", s.id=s.id, t.id=t.id, df=new.df )
  if (verbose) 
    pp("Creating a SpatialPointsTemporalDataFrame object",Sys.time())
  return( new("SpatialPointsTemporalDataFrame", spatial=sts, temporal=stt, data=stdf.new) )
}

as.data.frame.SpatialPointsTemporalDataFrame <- function(from) {
        df.new <- getDataFrame(from)
        ## now replace the s.id and t.id with the actual values.
        new.temporal.col <- getTimedatestamps(from)[ match( df.new$t.id, getTid(from) ) ]
        new.spatial.col <- getSpatialPoints(from)[ match( df.new$s.id, getSid(from) ), ]
        df.names <- names(df.new)
        df.names <- df.names[ !(df.names %in% c("t.id","s.id")) ]
        return( data.frame( df.new[df.names], temporal=new.temporal.col, spatial=new.spatial.col) )
}

setAs("SpatialPointsTemporalDataFrame", "data.frame",
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



setMethod("stDist",c(sp="SpatialPointsTemporalDataFrame",type="character"),
          function(sp,type,units, miles=TRUE){
            if ( !exists("type")) stop("Using stUpdate on an stDataFrame requires updating either type 'temporal' or type 'spatial'; none provided ")
            if (type == "spatial"){
              return(stDist(sp@spatial, units, miles=miles))
            } else if (type == "temporal"){
              return(stDist(sp@temporal, units))
            } else {
              stop("Using stUpdate on an stDataFrame requires updating either type 'temporal' or type 'spatial' ")
            }
          })

setMethod("stApply",c(st="SpatialPointsTemporalDataFrame", colname="character", format="character", fun="function", by.site="logical"),
          function(st, colname, format, fun, by.site){
            tds.formatted <- getTimedatestamps(st,format)
            tdsfm <- tds.formatted[match( st@data@df$"t.id", getTid(st) ) ]
            ## now, create a new df.
            if ( !by.site ){
              x <- data.frame( st@data@df[colname])
              return(gapply(x, which=1, FUN=fun, group=tdsfm, na.rm=TRUE) )
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
                out[[i]] <- gapply(x1, which=1, FUN=fun, group=st.grp, na.rm=TRUE)

              }
              return(out)
            }
            

          }
          )

setMethod("stJoin", c(x="SpatialPointsTemporalDataFrame", y="SpatialPointsTemporalDataFrame"),
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
