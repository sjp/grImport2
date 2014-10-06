\name{PathMoveTo-class}
\docType{class}
\alias{PathMoveTo-class}
\title{Class \code{"PathMoveTo"}}
\description{
  A description of a path move to operator (\code{"M"} in SVG parlance)
  that is meant to be used as part of a path.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{
      Object of class \code{"numeric"}. The x-location that the path is
      going to move to.
    }
    \item{\code{y}:}{
      Object of class \code{"numeric"}. The y-location that the path is
      going to move to.
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
  \code{\linkS4class{PathData}}, \code{\linkS4class{PathClosePath}},
  \code{\linkS4class{PathLineTo}}, \code{\linkS4class{PathCurveTo}}.
}
