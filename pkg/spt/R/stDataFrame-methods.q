validstDataFrameObject <- function(object) {
  ## TBD
  ## Want to make sure that only the s.id and t.id locations are included in the dataframe...
  ## and that s.id and t.id are integers?
  return(TRUE)
}
setValidity("stDataFrame", validstDataFrameObject)


setMethod("show", "stDataFrame",
          function(object){cat("A dataframe with",nrow(object@df),"observations and",ncol(object@df)-2,"data columns with\n",length(object@t.id),"unique timedates and\n",length(object@s.id),"unique locations.\n") } )


setMethod("stUpdate",c(st="stDataFrame", new.id="integer"),
          function(st, new.id, type=NA,...){
            ## Given that the dataframe will be reduced in either time or space,
            ## then the stDataFrame must be updated accordingly.
            if ( !exists("type")) stop("Using stUpdate on an stDataFrame requires updating either type 'temporal' or type 'spatial'; none provided ")
            if (type == "spatial"){
              new.s.id <- new.id
              new.s.id.indices <- which( st@s.id %in% new.s.id)
              new.df.indices <- (st@df$s.id %in% new.s.id)
              new.t.id <- unique(st@df$t.id[new.df.indices])
              ## add in some validity check here to make sure new.t.id will match new df$t.id
              return( new("stDataFrame", s.id=new.s.id, t.id=new.t.id, df=st@df[new.df.indices,]))
            } else if (type == "temporal"){
              new.t.id <- new.id
              new.t.id.indices <- which( st@t.id %in% new.t.id)
              new.df.indices <- (st@df$t.id %in% new.t.id)
              new.s.id <- unique(st@df$s.id[new.df.indices])
              ## add in some validity check here to make sure new.s.id will match new df$s.id
              st@df <- st@df[new.df.indices,]
              st@s.id <- new.s.id
              st@t.id <- new.t.id
              gc()
              return( st )
            } else {
              stop("Using stUpdate on an stDataFrame requires updating either type 'temporal' or type 'spatial' ")
            }
          }
          )

setMethod("stJoin",c(x="stDataFrame", y="stDataFrame"),
          function(x, y, x.maps, y.maps, new.tid, new.sid){
            ## In order to join two data frames, you have to know
            ## what the mapping between the original s.id (from x and y)
            ## and t.id (from x and y) are
            ## with the new joint s.id and t.id
            ## ??? FIX; TBD check for redundant points
            ## eg same time and date stamp,
            ## but possibly diff value.  eg replicates.
            ##
            ## First update the s.id and t.id in the dataframe
            x@df$"t.id" <- new.tid[ x.maps$tid.map[x@df$"t.id"] ]
            y@df$"t.id" <- new.tid[ y.maps$tid.map[y@df$"t.id"] ]
            x@df$"s.id" <- new.sid[ x.maps$sid.map[x@df$"s.id"] ]
            y@df$"s.id" <- new.sid[ y.maps$sid.map[y@df$"s.id"] ]
            ## Then update the s.id and t.id stTemporal object
            setTid(x) <- new.tid
            setSid(x) <- new.sid
            ## have to update the sid and tid columns of x and y's dataframes.
            ##
            setDataFrame(x) <- rbind( getDataFrame(x), getDataFrame(y))
            validObject(x)
            return( x )
          }
          )

## Getters
setMethod("getTid", signature(x="stDataFrame"),
          function(x) return(x@t.id) )
setMethod("getSid", signature(x="stDataFrame"),
          function(x) return(x@s.id) )
setMethod("getDataFrame", signature(x="stDataFrame"),
          function(x)return( x@df) )

## Setters
setReplaceMethod("setSid", signature("stDataFrame"),
          function(object,value){
            object@s.id <- value
            validObject(object)
            return(object)
          }
          )

setReplaceMethod("setTid", signature("stDataFrame"),
          function(object,value){
            object@t.id <- value
            validObject(object)
            return(object)
          }
          )

setReplaceMethod("setDataFrame", signature("stDataFrame"),
          function(object,value){
            object@df <- as.data.frame(value)
            return(object)
          }
          )
