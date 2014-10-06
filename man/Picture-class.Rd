\name{Picture-class}
\docType{class}
\alias{Picture-class}
\alias{[,Picture-method}
\alias{[[,Picture-method}
\alias{grobify,Picture-method}

\title{Class "Picture"}
\description{
  A collection of paths, polylines, rectangles and other graphical
  content and features that together describe a picture.
}
\section{Objects from the Class}{

  Objects can be created by calls of the form \code{new("Picture",
    ...)}.

}
\section{Slots}{
  \describe{
    \item{\code{content}:}{
      A list of objects of class \code{"PictureContent"} that are paths,
      groups rectangles and other related objects.
    }
    \item{\code{defs}:}{
      Object of class \code{"PictureDefinitions"} that contain all
      referenced content in the image. These are only used by the
      gridSVG package when "grobifying" an image.
    }
    \item{\code{summary}:}{
      Object of class \code{"PictureSummary"} that describes the scales
      applied to this picture.
    }
  }
}
\section{Methods}{
  \describe{
    \item{[}{
      \code{signature(x = "Picture")}: subset the content of a picture
      to produce a new picture (including a new, updated summary).
    }
    \item{[[}{
      \code{signature(x = "Picture")}: extract a single piece of a
      picture object, to produce a new picture (including a new, updated
      summary).
    }
    \item{grobify}{
      \code{signature(object = "Picture")}: convert a picture
      into a grid grob (for use as a one-off image).

      This method contains two additional arguments:

      \describe{
        \item{\code{gpFUN}}{
          This argument expects to take a function that modifies a
          \code{gpar} object. For its input, this function should take a
          \code{gpar} object, modify that object, and then return it. By
          default the value of this argument is the identity function.
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
\author{ Simon Potter }
\seealso{
  \code{\link{readPicture}},
  \code{\link{grid.picture}},
  \code{\link{PictureSummary-class}}.
}
\keyword{classes}
