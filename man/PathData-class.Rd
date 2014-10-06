\name{PathData-class}
\docType{class}
\alias{PathData-class}
\alias{applyTransform,PathData,matrix-method}

\title{Class \code{"PathData"}}
\description{
  An object that represents a sequence of path data segments in an SVG
  path.
}
\section{Slots}{
  \describe{
    \item{\code{segments}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{applyTransform}{\code{signature(object = "PathData", tm =
        "matrix")}: A transformation matrix to apply to all path
      segments in the path that \code{object} contains. Returns a new
      \code{"PathData"} object with each path segment transformed to
      their new locations.
    }
  }
}
\author{
  Simon Potter
}
\seealso{
  \code{\linkS4class{PathData}}, \code{\linkS4class{PathMoveTo}},
  \code{\linkS4class{PathLineTo}}, \code{\linkS4class{PathCurveTo}}.
}
