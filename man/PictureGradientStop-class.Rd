\name{PictureGradientStop-class}
\docType{class}
\alias{PictureGradientStop-class}
\alias{grobify,PictureGradientStop-method}

\title{Class \code{"PictureGradientStop"}}
\description{
  A description of a gradient stop that is meant to be used as part of a
  larger picture (as it does not directly draw anything itself). This
  object will have no effect unless it is used in conjunction with the
  gridSVG package.
}
\section{Slots}{
  \describe{
    \item{\code{offset}:}{
      Object of class \code{"numeric"}. The offset (usually between 0
      and 1) from the start of the gradient. These represent locations
      to place the colours (\code{col}) at.
    }
    \item{\code{col}:}{
      Object of class \code{"character"}. A colour in the form
      \code{"#RRGGBBAA"}.
    }
  }
}
\section{Methods}{
  \describe{
    \item{grobify}{\code{signature(object = "PictureGradientStop")}:
      convert to a gradient stop object for use with the gridSVG
      package.
    }
  }
}
\author{
  Simon Potter
}
\seealso{
  \code{\linkS4class{PictureLinearGradient}},
  \code{\linkS4class{PictureRadialGradient}}.
}
