\name{stTemporal-class}
\docType{class}
\alias{stTemporal-class}
\alias{print, stTemporal-method}
\alias{summary,stTemporal-method}
\alias{show,stTemporal-method}
\alias{stDist,stTemporal-method}
\alias{stSubset,stTemporal-method}
\alias{stJoin,stTemporal-method}
\alias{stUpdate,stTemporal-method}
\alias{getDataFrame,stTemporal-method}
\alias{getTid,stTemporal-method}
\alias{setTid,stTemporal-method}
\alias{getTimedatestamps,stTemporal-method}
\alias{setTimedatestamps,stTemporal-method}

\title{Class \code{"stTemporal"} }
\description{ A class to hold the temporal part of spatiotemporal data.
By construction a valid object has no repeated unique ids and no
repeated time stamps.  This is checked for during object creation and
coercion. }
\section{Objects from the Class \code{stTemporal} }{
 meaningful }
\section{Slots}{
  \describe{
    \item{\code{t.id}:}{Object of class \code{"integer"}; the unique
      identifiers for each time date stamp.  A vector of the same length
    as timedatestamps slot.  }
    \item{\code{timedatestamps}:}{Object of class \code{"timeDate"}; the
    unique time stamps for observations in the stDataFrame, }
  }
}
\section{Methods}{
  \describe{
    \item{getTid}{\code{signature(obj = "stTemporal")}: returns the
      unique temporal ids for each time stamp
    }
    \item{setTid}{\code{signature("stTemporal")}:  sets the
      unique temporal ids for each time stamp
    }
    \item{getTimedatestamps}{\code{signature(obj = "stTemporal")}:
      returns the unique time stamps in the object.
    }
    \item{setTimedatestamps}{\code{signature("stTemporal")}: sets the
      unique time stamps }
    \item{show}{\code{signature(obj = "stTemporal")}: Returns the number
      of (unique) time stamps
    }
    \item{stDist}{\code{signature(obj = "stTemporal",
	units="character")}: returns the distance matrix between the
      temporal points in units given, one of days, weeks, months, etc.
      Uses difftimDate, see that for more info. }
    \item{stSubset}{\code{signature(obj = "stTemporal", bounds="ANY")}:
      Given character or timeDate bounds (vectors of length 2), it
      returns the subset of the stTemporal object bounded by the bounds.
      If the bounds are character, then a format must also be included.
    }
    \item{stJoin}{\code{signature(obj1 = "stTemporal", obj2 =
	"stTemporal")}: Given 2 stTemporal objects, a new stTemporal
      object is created.  Returned is a list with 3 elements.  The
      first, "stTemporal" is the new stTemporal object.  The second is
      the mapping of the original t.ids for obj1 to the t.ids in the new
      object;  The third is the mapping of the original t.ids
      for obj2 to the t.ids in the new object
    }
    \item{stUpdate}{\code{signature(obj = "stTemporal", rm.id="integer")}: Used
      internally to remove components of the object indexed by t.ids. }
  }
}

\author{ J. Blair Christian, \email{blair.christian@gmail.com} }
\note{  
  This object's primary purpose is to keep the temporal information
  organized for the \code{SpatialTemporalDataFrame} class.  The
  \code{stDataFrame} class has the data to which the \code{t.id} are mapped.
}

\section{Warning }{
  We recommend avoiding use of the setters.  They were
  originally for error checking.
}

\seealso{
  \code{\link{SpatialTemporalDataFrame-class}},
  \code{\link{stDataFrame-class}},
  \code{\link{strptime}},
  \code{\link{difftimeDate}}
}

\examples{
  x <- stTemporal(timeVals=c("2008-09-29","2009-01-14","2012-12-12"),format="%Y-%m-%d")
summary(x)
show(x)
getTid(x)
getTimedatestamps(x)
setTid(x) <- as.integer(c( 843, 211, 579) )

stDist(x,"days")
stDist(x,"weeks")

show(x)
getTid(x)
getTimedatestamps(x)
getTimedatestamps(x, format="%m/%d/%Y")

## Different behavior given integer, timeDate, character.
x1 <- stSubset( x, as.integer(c(843,579) ) )
show(x1)

stSubset( x, timeDate(c("01/01/2009", "01/01/2010"), format="%m/%d/%Y"))

stSubset( x, c("01/01/2009", "01/01/2010"), format="%m/%d/%Y")


y <- stTemporal( timeVals=c("01/01/1970"), format="%m/%d/%Y")

# note that unique ids are not currently preserved
xy <- stJoin(x,y)
class(xy)
class(xy$stTemporal)


}

\keyword{classes}
