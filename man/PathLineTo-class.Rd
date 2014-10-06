\name{PathLineTo-class}
\docType{class}
\alias{PathLineTo-class}
\title{Class \code{"PathLineTo"}}
\description{
  A description of a path line to operator (\code{"L"} in SVG parlance)
  that is meant to be used as part of a path.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{
      Object of class \code{"numeric"}. The x-location that the path is
      going to draw a line to.
    }
    \item{\code{y}:}{
      Object of class \code{"numeric"}. The y-location that the path is
      going to draw a line to.
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
  \code{\linkS4class{PathClosePath}}, \code{\linkS4class{PathCurveTo}}.
}
