\name{PictureRect-class}
\docType{class}
\alias{PictureRect-class}
\alias{applyTransform,PictureRect,matrix-method}
\alias{grobify,PictureRect-method}

\title{Class \code{"PictureRect"}}
\description{
  A description of a rectangle.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the x-location of the top-left corner of the
      rectangle.
    }
    \item{\code{y}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the y-location of the top-left corner of the
      rectangle.
    }
    \item{\code{width}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the width of the rectangle.
    }
    \item{\code{height}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the height of the rectangle.
    }
    \item{\code{angle}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the angle applied to the image. Corresponds to grid's
      viewport angles.
    }
    \item{\code{gp}:}{
      Object of class \code{"gpar"}. A grid \code{gpar} object.
    }
    \item{\code{bbox}:}{
      Object of class \code{"numeric"}. Represented as \code{[xmin,
      xmax, ymin, ymax]}. The bounding box of the rectangle. Used for
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
      \code{signature(object = "PictureRect", tm = "matrix")}:
      transforms the locations described by the rectangle object by a
      3x3 transformation matrix and returns a \code{"PictureRect"}
      object with the transformed locations.
    }
    \item{grobify}{\code{signature(object = "PictureRect")}:
      converts the object into a grid rectangle grob.

      While not intended to be used directly, this method contains three
      arguments:

      \describe{
        \item{\code{defs}}{
          An object of class \code{"PictureDefinitions"} that contains
          definitions of graphical content, primarily for use with the
          gridSVG package.
        }
        \item{\code{gpFUN}}{
          This argument takes a function that should expect to take a
          single \code{gpar} object and return a modified \code{gpar}
          object. By default, the value of this argument is the identity
          function.
        }
        \item{\code{ext}}{
          A character vector. See \code{\link{grid.picture}} for more
          information on what this extension selection parameter means,
          in addition to the valid values this argument takes.
        }
      }
    }
  }
}
\author{
  Simon Potter
}
\seealso{
  \code{\linkS4class{PicturePath}}.
}
