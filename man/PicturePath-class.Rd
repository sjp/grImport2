\name{PicturePath-class}
\docType{class}
\alias{PicturePath-class}
\alias{applyTransform,PicturePath,matrix-method}
\alias{grobify,PicturePath-method}

\title{Class \code{"PicturePath"}}
\description{
  A description of a path.
}
\section{Slots}{
  \describe{
    \item{\code{d}:}{
      Object of class \code{"PathData"}. A description of how the path
      draws and where it draws.
    }
    \item{\code{rule}:}{
      Object of class \code{"character"}. A single element vector
      specifying the fill rule of the path.
    }
    \item{\code{gp}:}{
      Object of class \code{"gpar"}. A grid \code{gpar} object that
      describes graphical parameters applied to the path.
    }
    \item{\code{bbox}:}{
      Object of class \code{"numeric"}. The bounding box of the
      path. Represented as \code{[xmin, xmax, ymin, ymax]}. Used for
      features such as clipping.
    }
  }
}
\section{Extends}{
  Class \code{linkS4class{PictureContent}}, directly.
}
\section{Methods}{
  \describe{
    \item{applyTransform}{
      \code{signature(object = "PicturePath", tm = "matrix")}:
      transforms each location described by the path's data by a 3x3
      transformation matrix and returns a new \code{"PicturePath"}
      object with the newly transformed locations.
    }
    \item{grobify}{
      \code{signature(object = "PicturePath")}: converts the path
      description into a grid grob that represents the path.

      Although not expected to be called directly, this method contains
      three arguments.

      \describe{
        \item{\code{defs}}{
          An object of class \code{"PictureDefinitions"}. This object
          holds definitions of advanced graphical content, primarily for
          use with the gridSVG package.
        }
        \item{\code{gpFUN}}{
          This argument expects to take a function that modifies a
          \code{gpar} object. This function should take a \code{gpar}
          object and return a modified \code{gpar} object. By default
          this argument is the identity function.
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
  \code{\linkS4class{PictureRect}}, \code{\linkS4class{PathData}}.

}
