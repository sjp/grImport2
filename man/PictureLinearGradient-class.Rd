\name{PictureLinearGradient-class}
\docType{class}
\alias{PictureLinearGradient-class}
\alias{applyTransform,PictureLinearGradient,matrix-method}
\alias{grobify,PictureLinearGradient-method}
\title{Class "PictureLinearGradient" }
\description{
  A description of a linear gradient that is meant to be used as part of
  a larger picture (as it does not directly draw anything itself). This
  object will have no effect unless it is used in conjunction with the
  gridSVG package.
}
\section{Slots}{
  \describe{
    \item{\code{x0}:}{
      Object of class \code{"numeric"}. The starting x-location of the
      linear gradient.
    }
    \item{\code{y0}:}{
      Object of class \code{"numeric"}. The starting y-location of the
      linear gradient.
    }
    \item{\code{x1}:}{
      Object of class \code{"numeric"}. The ending x-location of the
      linear gradient.
    }
    \item{\code{y1}:}{
      Object of class \code{"numeric"}. The ending y-location of the
      linear gradient.
    }
    \item{\code{spreadMethod}:}{
      Object of class \code{"character"} that specifies what happens
      when a gradient ends within its bounds. Must be one of "pad",
      "reflect" or "repeat". See \code{"linearGradient"} in the gridSVG
      package for more information.
    }
    \item{\code{stops}:}{
      Object of class \code{"list"} that is a list of objects of class
      \code{"PictureGradientStop"}.
    }
  }
}
\section{Extends}{
  Class \code{"PictureContent"}.
}
\section{Methods}{
  \describe{
    \item{applyTransform}{
      \code{signature(object = "PictureLinearGradient", tm = "matrix")}:
      transform the locations that represent the bounds and direction of
      the gradient by a 3x3 transformation matrix.
    }
    \item{grobify}{\code{signature(object = "PictureLinearGradient")}:
      convert to a gradient object for use with the gridSVG package.
    }
  }
}
\author{ Simon Potter }
\seealso{
  \code{\link{Picture-class}},
  \code{\link{grid.picture}}.
}
\keyword{classes}
