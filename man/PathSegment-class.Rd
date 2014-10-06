\name{PathSegment-class}
\docType{class}
\alias{PathSegment-class}
\alias{applyTransform,PathSegment,matrix-method}

\title{Class \code{"PathSegment"}}
\description{
  A segment of a path that is not intended to be used directly, merely
  as a convenience base class for path segment operators.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{Object of class \code{"numeric"}. An x-location.}
    \item{\code{y}:}{Object of class \code{"numeric"}. A y-location.}
  }
}
\section{Methods}{
  \describe{
    \item{applyTransform}{\code{signature(object = "PathSegment", tm =
        "matrix")}: When given a 3x3 numeric transformation matrix, the
      points in the segment are transformed, and produce a new object
      representing a path segment. Not intended to be used directly on
      path segments, but objects that inherit from path segments
      (e.g. the \code{"M"}, \code{"L"}, \code{"C"}, and \code{"Z"}
      operators.
    }
  }
}
\author{
  Simon Potter
}
\seealso{
  \code{\linkS4class{PathMoveTo}}, \code{\linkS4class{PathClosePath}},
  \code{\linkS4class{PathLineTo}}, \code{\linkS4class{PathCurveTo}}.
}
