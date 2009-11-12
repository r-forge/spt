
setMethod("initialize", "stTemporal",
          function(.Object, timedatestamps, t.id){
            .Object@timedatestamps <- timedatestamps
            .Object@t.id <- t.id
            return(.Object)
          }
          )

## assign the function as the validity method for the class
validstTemporalObject <- function(object) {
  if(length(object@t.id) != length(object@timedatestamps))
    paste("Unequal t.id, timeDate lengths: ", length(object@t.id), ", ", length(object@timedatestamps), sep="")
  if ( !identical( class(object@t.id), "integer"))
    paste("The class of t.id must be 'integer', was given as '",class(object@t.id),"'",sep="")
  if ( !identical( class(object@timedatestamps)[[1]], "timeDate") )
    paste("The class of timedatestamps must be 'timeDate', was given as '",class(object@timedatestamps),"'",sep="")
  return(TRUE)
}
setValidity("stTemporal", validstTemporalObject)

setMethod("show", "stTemporal",
          function(object) return( getDataFrame(object))  )

setMethod("summary", "stTemporal",
          function(object){cat("A vector of timeDates of length",length(object@t.id),"\n") } )

## Getters
setMethod("getTid", signature(x="stTemporal"),
          function(x) return(x@t.id) )

setMethod("getTimedatestamps", signature(x="stTemporal"),
          function(x, format="%Y-%m-%d")return( format(x@timedatestamps, format=format)))

setMethod("getDataFrame", signature(x="stTemporal"),
          function(x, format="%Y-%m-%d", time.type="char") {
            if (time.type=="char"){
              return(data.frame(tid=x@t.id, timedatestamps=format(x@timedatestamps,format)))
            } else {
              return(data.frame(tid=x@t.id, timedatestamps=x@timedatestamps) )
            }
          }
          )

## Deprecated; moved to getTimedatestamps
#### Given a format string, return the t.id which match it?
##setMethod("getTimeByFormat", signature(x="stTemporal"),
##          function(x,format)return( format(x@timedatestamps, format=format)) )


## Setters
setReplaceMethod("setTid", signature("stTemporal"),
          function(object, value){
            object@t.id <- value
            #validObject(x)
            return(object)
          }
          )
setReplaceMethod("setTimedatestamps", signature("stTemporal"),
          function(object, value){
            validObject(value)
            object@timedatestamps <- value
            return(object)
          }
          )



## Constructor
stTemporal <- function(timeVals, format=NULL, t.id=as.integer(1:length(timeVals)) ){
  td <- timeDate(timeVals, format)
  return(new("stTemporal", timedatestamps=td, t.id=t.id))
}

## need to do anthoer subset for signature x="character" instead of timedate?
setMethod("stSubset", signature(x="stTemporal", bounds="timeDate"),
          function(x,bounds){
            new.members <- which( x@timedatestamps >= bounds[1] & x@timedatestamps < bounds[2])
            x@t.id <- x@t.id[new.members]
            x@timedatestamps <- x@timedatestamps[new.members]
            return( x )
          }
          )
setMethod("stSubset", signature(x="stTemporal", bounds="character"),
          function(x, bounds, format=NA,...){
            ## Have to figure out length of bounds.
            ## if it's 1, then the user wants to match by
            ## say "2001-01", which is jan 2001 only.
            if (length(bounds) == 1){
              new.tds <- format(x@timedatestamps, format=format)
              new.members <- which( new.tds %in% bounds)
              x@t.id <- x@t.id[new.members]
              x@timedatestamps <- x@timedatestamps[new.members]
              return( x )
              ## Otherwise, if the length is 2, then they
              ## want a temporal subset.
            } else if (length(bounds)==2) {
              return(stSubset(x, timeDate(bounds,format)))
            } else {
              stop("stSubset on an stTemporal object requires either 1 or 2 bounds.")
            }
          }
          )

setMethod("stSubset", signature(x="stTemporal", bounds="integer"),
          function(x,bounds){
            new.members <- which( x@t.id %in% bounds)
            x@t.id <- x@t.id[new.members]
            x@timedatestamps <- x@timedatestamps[new.members]
            return( x )
          }
          )

setMethod("stSubset", signature(x="stTemporal", bounds="numeric"),
          function(x,bounds){
            if ( bounds-as.integer(bounds)==0){
              return( stSubset(x, as.integer(bounds)) )
            } else {
              stop("bounds must be an integer")
            }
          }
          )

## FIX: as.xts doesn't work on timeDate class.
##setMethod("periodicity", signature(x="stTemporal"),
##          function(x) return( periodicity( getTimedatestamps(x) ) )  )
          

setMethod("stUpdate",c(st="stTemporal", new.id="integer"),
          function(st, new.id){
            new.t.id <- new.id
            new.members <- which( st@t.id %in% new.t.id)
            ## TBD if new.members is NA or empty, then what?

            ## choices: create new object, or just change current object?
            st@timedatestamps <- st@timedatestamps[new.members]
            st@t.id <- new.t.id
            gc()
            return( st )
          }
          )


setMethod("stDist", c(sp="stTemporal",type="character"),
          function(sp,type){
            ## Here type is units, days, hours, etc.
            n <- length(sp@t.id)
            td.dist <- matrix(0,n,n)
            for (i in 2:n){
              for (j in 1: (n-1) ){
                td.dist[j,i] <- td.dist[i,j] <- difftimeDate(sp@timedatestamps[i], sp@timedatestamps[j], units=type)
              }
            }
            colnames(td.dist) <- getTid(sp)
            rownames(td.dist) <- getTid(sp)
            return(td.dist)
          }
          )

setMethod("stJoin",c(x="stTemporal",y="stTemporal"),
          function(x,y){
            ## Issue new unique IDs?
            ## Assumes x, y in order??
            orig.x <- getTimedatestamps(x)
            orig.y <- getTimedatestamps(y)
            new.tds <- sort(unique( c(orig.x, orig.y) ) )
            x.id.map <- match(orig.x, new.tds)
            y.id.map <- match(orig.y, new.tds)
            setTid(x) <- as.integer(1:length(new.tds)) 
            setTimedatestamps(x) <- timeDate(new.tds) 
            return(list(stTemporal=x, x.id.map=x.id.map, y.id.map=y.id.map))
          }
          )

## had problem with initializing after setClass("stTemporal") a lot...
## don't know why.
##
##setMethod("initialize", "stTemporal", function(.Object, ...) {
##  .Object <- callNextMethod()
##  if(length(.Object@t.id) != length(.Object@timedatestampsstamp))
##    stop("specified t.id and timedatestamp of different lengths")
##  .Object
##})

##getMethod("initialize","stTemporal")
