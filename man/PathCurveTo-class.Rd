\name{PathCurveTo-class}
\docType{class}
\alias{PathCurveTo-class}
\title{Class \code{"PathCurveTo"}}
\description{
  A description of a path curve to operator (\code{"C"} in SVG parlance)
  that is meant to be used as part of a path.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{
      Object of class \code{"numeric"}. A vector of length four that
      represent the x-locations of the control points of a cubic bezier
      curve.
    }
    \item{\code{y}:}{
      Object of class \code{"numeric"}. A vector of length four that
      represent the y-locations of the control points of a cubic bezier
      curve.
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
  \code{\linkS4class{PathLineTo}}, \code{\linkS4class{PathClosePath}}.
}
