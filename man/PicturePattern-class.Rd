\name{PicturePattern-class}
\docType{class}
\alias{PicturePattern-class}
\alias{applyTransform,PicturePattern,matrix-method}
\alias{grobify,PicturePattern-method}

\title{Class \code{"PicturePattern"}}
\description{
  A description of a pattern to be referenced (and used) by graphical
  content.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the x-location of the top-left corner of the
      pattern.
    }
    \item{\code{y}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the y-location of the top-left corner of the
      pattern.
    }
    \item{\code{width}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the width of the pattern.
    }
    \item{\code{height}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the height of the pattern.
    }
    \item{\code{angle}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the angle applied to the image. Corresponds to grid's
      viewport angles.
    }
    \item{\code{definition}:}{
      Object of class \code{"list"}. A list of \code{"PictureContent"}
      objects that define the content of the pattern "tile".
    }
  }
}
\section{Extends}{
  Class \code{\linkS4class{PictureContent}}, directly.
}
\section{Methods}{
  \describe{
    \item{applyTransform}{
      \code{signature(object = "PicturePattern", tm = "matrix")}:
      transforms the locations described by the pattern object by a
      3x3 transformation matrix and returns a \code{"PicturePattern"}
      object with the transformed locations.
    }
    \item{grobify}{\code{signature(object = "PicturePattern")}:
      convert the pattern description into a gridSVG pattern
      object. Useful only in conjunction with the gridSVG package. The
      pattern object will store the definition of the pattern (almost
      certainly a raster of some sort) as a grob too.

      While not intended to be used directly, this method contains a
      single optional argument. This argument is \code{ext}, a character
      vector. See \code{\link{grid.picture}} for more information on
      what this extension selection parameter means, in addition to the
      valid values this argument takes.
    }
  }
}
\author{
  Simon Potter
}
