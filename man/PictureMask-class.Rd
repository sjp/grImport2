\name{PictureMask-class}
\docType{class}
\alias{PictureMask-class}
\alias{grobify,PictureMask-method}

\title{Class \code{"PictureMask"}}
\description{
  A description of a luminance mask.
}
\section{Slots}{
  \describe{
    \item{\code{content}:}{
      Object of class \code{"list"}. A list of \code{"PictureContent"}
      objects that are use to define the mask. Typically objects that
      actually draw content (e.g. a path or a rect).
    }
  }
}
\section{Extends}{
  Class \code{\linkS4class{PictureContent}}, directly.
}
\section{Methods}{
  \describe{
    \item{grobify}{
      \code{signature(object = "PictureMask")}: creates a mask object
      for use with the gridSVG package. Not intended to be used
      directly.

      There are three arguments to this \code{grobify} method:

      \describe{
        \item{\code{defs}}{
          An object of class \code{"PictureDefinitions"}. This object is
          primarily useful in conjunction with the gridSVG package.
        }
        \item{\code{gpFUN}}{
          This argument takes a function that takes a \code{gpar} object
          and returns a modified \code{gpar} object. The default value
          is the identity function.
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
