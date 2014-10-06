\name{PictureFilter-class}
\docType{class}
\alias{PictureFilter-class}
\alias{grobify,PictureFilter-method}

\title{Class \code{"PictureFilter"}}
\description{
  A description of a filter effect. To be used in conjunction with the
  gridSVG package.
}
\section{Slots}{
  \describe{
    \item{\code{filterUnits}:}{
      Object of class \code{"character"}. The units that the filter
      effect should be positioned against. This should always be
      \code{"bbox"}.
    }
    \item{\code{x}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the x-location of the bottom-left corner of the
      filter effect region.
    }
    \item{\code{y}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the y-location of the bottom-left corner of the
      filter effect region.
    }
    \item{\code{width}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the width of the filter effect region.
    }
    \item{\code{height}:}{
      Object of class \code{"numeric"}. A vector of length one
      representing the height of the filter effect region.
    }
    \item{\code{content}:}{
      Object of class \code{"PictureFeColorMatrix"}. A description of
      the colour transformation matrix applied to a filter target.
    }
  }
}
\section{Extends}{
  Class \code{\linkS4class{PictureContent}}, directly.
}
\section{Methods}{
  \describe{
    \item{grobify}{
      \code{signature(object = "PictureFilter")}: Creates a filter
      effect object for use with the gridSVG package.
    }
  }
}
\author{
  Simon Potter
}
\seealso{
  \code{\linkS4class{PictureFeColorMatrix}}
}
