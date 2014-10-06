\name{grobify}
\alias{grobify}
\title{
  Convert a Picture Object into a grid grob
}
\description{
  Converts \code{"Picture"} objects and pieces of a picture
  (i.e. objects inheriting from \code{"PictureContent"} objects) into
  grid grobs.
}
\usage{
grobify(object, ...)
}
\arguments{
  \item{object}{
    An object that contains \code{grobify} methods. Typically a
    \code{"Picture"} object.
  }
  \item{\dots}{
    Further arguments to specific \code{grobify} methods.
  }
}
\details{
  Although not required by some grobify methods, there are further
  arguments that may be used, which will be described here.

  \describe{
    \item{\code{defs}}{
      This argument should take a \code{"PictureDefinitions"} object
      that contains definitions of referenced content. This is only useful
      when the \code{gridSVG} is set to \code{TRUE}.
    }
    \item{\code{clip}}{

      This argument is supplied in the form of a character vector. It
      usually only applies when grobifying a \code{"PictureGroup"}
      object. There are three possible values for \code{clip}:

      \describe{
        \item{\code{none}}{
          No clipping will be applied to the object.
        }
        \item{\code{bbox}}{
          The clipping will be applied to the rectangular region that
          bounds the clipping path's content. This argument is
          particularly useful as R graphics can only use rectangular
          clipping regions. If we want more complex clipping regions,
          the value of \code{gridSVG} must be used instead.
        }
        \item{\code{gridSVG}}{
          When this value is set, we are no longer restricted to
          clipping to rectangular regions, and many images will require
          it to be set to display correctly. This argument requires the
          gridSVG package to be installed.
        }
      }
    }
    \item{\code{gridSVG}}{
      This is a logical value that, when \code{TRUE}, allows graphical
      features to be drawn that are not possible in R graphics. This
      requires the gridSVG package. When this argument is \code{TRUE}
      we can correctly import and render patterns, masks, filters and
      gradients.
    }
  }
}
\value{
  A grid grob.
}
\author{
  Simon Potter
}
