\name{PictureRadialGradient-class}
\docType{class}
\alias{PictureRadialGradient-class}
\alias{applyTransform,PictureRadialGradient,matrix-method}
\alias{grobify,PictureRadialGradient-method}
\title{Class "PictureRadialGradient" }
\description{
  A description of a radial gradient that is meant to be used as part of
  a larger picture (as it does not directly draw anything itself). This
  object will have no effect unless it is used in conjunction with the
  gridSVG package.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{
      Object of class \code{"numeric"}. The x-location of the radial
      gradient.
    }
    \item{\code{y}:}{
      Object of class \code{"numeric"}. The y-location of the radial
      gradient.
    }
    \item{\code{r}:}{
      Object of class \code{"numeric"}. The radius of the radial
      gradient.
    }
    \item{\code{fx}:}{
      Object of class \code{"numeric"}. The x-location of the focal
      point of the radial gradient.
    }
    \item{\code{fy}:}{
      Object of class \code{"numeric"}. The y-location of the focal
      point of the radial gradient.
    }
    \item{\code{spreadMethod}:}{
      Object of class \code{"character"} that specifies what happens
      when a gradient ends within its bounds. Must be one of "pad",
      "reflect" or "repeat". See \code{"radialGradient"} in the gridSVG
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
      \code{signature(object = "PictureRadialGradient", tm = "matrix")}:
      transform the locations that represent the bounds and direction of
      the gradient by a 3x3 transformation matrix.
    }
    \item{grobify}{\code{signature(object = "PictureRadialGradient")}:
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
