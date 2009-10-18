\name{stSpatial-class}
\docType{class}
\alias{stSpatial-class}
\alias{Spatial}
\alias{print.summary.Spatial}
\alias{summary,Spatial-method}
\alias{plot,Spatial,missing-method}

\title{Class \code{"stSpatial"} }
\description{ }
\section{Objects from the Class}{
  This is a virtual class. You cannot create an object of this class,
  only those that extend it.   It extends the
  \code{\link{Spatial-class}} from the \code{sp} library.   Currently,
  it is only extended by stSpatialPoints,
  but stSpatialGrid and stSpatialPolygons (and maybe even
  stSpatialLines) will be added. }
\section{Slots}{
  \describe{
    \item{\code{s.id}:}{Object of class \code{"integer"}; the unique
      identifiers for each spatial location.  A vector of the same length
    as the number of spatial .  }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; inherited from
      the class Spatial}
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; inherited from
      the class Spatial}
  }
}
\section{Methods}{
  \describe{
    \item{}{\code{signature(obj = "")}:  }
    \item{}{\code{signature(obj = "")}:  }
    \item{}{\code{signature(obj = "")}:  }
    \item{}{\code{signature(obj = "")}:  }
    \item{}{\code{signature(obj = "")}:  }
  }
}

\usage{
Spatial(bbox, proj4string = CRS(as.character(NA)))
}

\arguments{
\item{}{}
\item{}{}
\item{}{}
}

\author{ J. Blair Christian, \email{blair.christian@gmail.com} }
\note{  

}

\section{Warning }{
}

\seealso{

  \code{\link{SpatialPoints-class}},
  \code{\link{Spatial-class}},
  \code{\link{stSpatialPoints-class}},
}
%\examples{} virtual!

\keyword{classes}
