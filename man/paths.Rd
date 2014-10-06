\name{Path Segments}
\alias{moveTo}
\alias{lineTo}
\alias{curveTo}
\alias{closePath}
\title{
  Convenience Functions for Path Segments
}
\description{
  These functions are convenience constructors for path segment objects
  of classes \code{"PathMoveTo"}, \code{"PathLineTo"},
  \code{"PathCurveTo"} and \code{"PathClosePath"}.
}
\usage{
moveTo(x, y)
lineTo(x, y)
curveTo(x, y)
closePath(x, y)
}
\arguments{
  \item{x}{
    A numeric vector representing x-locations.
  }
  \item{y}{
    A numeric vector representing y-locations.
  }
}
\details{
  For \code{x} and \code{y}, they should each be of length one so that
  they represent a single point location except in the case of
  \code{curveTo}. In the case of \code{curveTo}, it expects \code{x} and
  \code{y} vectors of length four, which represent control points in a
  cubic bezier curve.
}
\value{
  For \code{moveTo}, an object of class \code{"PathMoveTo"}.

  For \code{lineTo}, an object of class \code{"PathLineTo"}.

  For \code{curveTo}, an object of class \code{"PathCurveTo"}.

  For \code{closePath}, an object of class \code{"PathClosePath"}.
}
\author{
  Simon Potter
}
