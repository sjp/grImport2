\name{PathClosePath-class}
\docType{class}
\alias{PathClosePath-class}

\title{Class \code{"PathClosePath"}}
\description{
  A description of a path close path operator (\code{"Z"} in SVG
  parlance) that is meant to be used as part of a path.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{
      Object of class \code{"numeric"}. The x-location that the path is
      going to close to.
    }
    \item{\code{y}:}{
      Object of class \code{"numeric"}. The y-location that the path is
      going to close to.
    }
  }
}
\section{Extends}{
  Class \code{\linkS4class{PathSegment}}, directly.
}
\author{
  Simon Potter
}
\seealso{
  \code{\linkS4class{PathData}}, \code{\linkS4class{PathMoveTo}},
  \code{\linkS4class{PathLineTo}}, \code{\linkS4class{PathCurveTo}}.
}
