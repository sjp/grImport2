\name{PictureImage-class}
\docType{class}
\alias{PictureImage-class}
\alias{applyTransform,PictureImage,matrix-method}
\alias{grobify,PictureImage-method}

\title{Class \code{"PictureImage"}}
\description{
  A description of a raster image.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the x-location of the top-left corner of the
      image.
    }
    \item{\code{y}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the y-location of the top-left corner of the
      image.
    }
    \item{\code{width}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the width of the image.
    }
    \item{\code{height}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the height of the image.
    }
    \item{\code{image}:}{
      Object of class \code{"nativeRaster"}. A description of the raster
      image as an array of integers representing pixel colours. Not
      intended to be created directly, but as a result of calling the
      \code{jpeg} and \code{png} packages \code{read*()} functions.
    }
    \item{\code{maskRef}:}{
      Object of class \code{"ANY"}. A character reference to an object
      that will mask this image. Not intended to be used directly (so
      can be \code{NULL}.
    }
    \item{\code{bbox}:}{
      Object of class \code{"numeric"}. Represented as \code{[xmin,
      xmax, ymin, ymax]}. The bounding box of the image. Used for
      features such as clipping.
    }
  }
}
\section{Extends}{
  Class \code{\linkS4class{PictureContent}}, directly.
}
\section{Methods}{
  \describe{
    \item{applyTransform}{
      \code{signature(object = "PictureImage", tm = "matrix")}:
      transforms each location described by the image by a 3x3
      transformation matrix and returns a new \code{"PictureImage"}
      object with the newly transformed locations.
    }
    \item{grobify}{
      \code{signature(object = "PictureImage")}: converts the image
      description into a grid raster grob that represents the
      image. Typically used in conjunction with a pattern.

      While not intended to be used directly, this function contains a
      single optional argument, \code{ext}, which is a character vector.
      See \code{\link{grid.picture}} for more information on what this
      extension selection parameter means, in addition to the valid
      values this argument takes.
    }
  }
}
\author{
  Simon Potter
}
\seealso{
  \code{\linkS4class{PictureRect}}.
}
