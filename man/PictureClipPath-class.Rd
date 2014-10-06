\name{PictureClipPath-class}
\docType{class}
\alias{PictureClipPath-class}
\alias{grobify,PictureClipPath-method}

\title{Class \code{"PictureClipPath"}}
\description{
  A description of a clipping path that is meant to be used as part of a
  larger picture (as it does not directly draw anything itself). This is
  primarily used for determining the bounding box of the clipping region
  (because R graphics only supports rectangular clipping regions). This
  object will have a greater effect with it is used in conjunction with
  the gridSVG package as it can apply non-rectangular clipping regions.
}
\section{Slots}{
  \describe{
    \item{\code{content}:}{
      Object of class \code{"list"}. A list of \code{"PictureContent"}
      objects that describe the outline(s) of the clipping region(s).
    }
    \item{\code{label}:}{
      Object of class \code{"character"}. A label to identify the
      clipping path by so that it may be referred to by content within
      the image.
    }
  }
}
\section{Extends}{
  Class \code{\linkS4class{PictureContent}}, directly.
}
\section{Methods}{
  \describe{
    \item{grobify}{\code{signature(object = "PictureClipPath")}:
      convert the clipping path into a grid grob. Useful only in
      conjunction with the gridSVG package for non-rectangular paths.

      This \code{grobify} method takes three arguments:

      \describe{
        \item{\code{defs}}{
          Required. This argument expects a \code{"PictureDefinitions"}
          object that contains referenced content.
        }
        \item{\code{gpFUN}}{
          A function that takes a \code{gpar} object and returns a
          modified \code{gpar} object. The default value for this
          arugment is the identity function.
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
  \code{\linkS4class{PictureGroup}}.
}
