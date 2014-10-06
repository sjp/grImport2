\name{grid.symbols}
\alias{symbolsGrob}
\alias{grid.symbols}
\title{
  Draw a Picture Object as Data Symbols
}
\description{
  These functions take a \code{"Picture"} object and either draw the
  picture at several locations or create a grid graphical object
  representing the picture (drawn at several locations).
}
\usage{
symbolsGrob(picture,
            x = stats::runif(10), y = stats::runif(10),
            size = unit(1, "char"),
            default.units = "native",
            gpFUN = identity,
            ext = c("none", "clipbbox", "gridSVG"),
            prefix = NULL,
            ..., name = NULL)
grid.symbols(...)
}
\arguments{
  \item{picture}{
    A \code{"Picture"} object.
  }
  \item{x}{
    A numeric vector or unit object specifying x-locations.
  }
  \item{y}{
    A numeric vector or unit object specifying y-locations.
  }
  \item{size}{
    A numeric vector or unit object specifying symbol sizes.
  }
  \item{default.units}{
    A string indicating the default units to use if \code{x}, \code{y},
    \code{width}, or \code{height} are only given as numeric vectors.
  }
  \item{gpFUN}{
    A function that takes a grid \code{gpar} object and returns a
    (possibly modified) \code{gpar} object.
  }
  \item{ext}{
    A character vector. Selects from one of three possible extensions
    for drawing imported pictures. \code{"none"} means that no clipping
    will be applied to the imported picture. \code{"clipbbox"} means
    that clipping will be applied, but only to the bounding boxes of any
    imported clipping paths. \code{"gridSVG"} means that gridSVG will be
    used when drawing the \code{"Picture"} object, which enables the use
    of complex clipping paths, gradients, patterns, etc. to be rendered
    from an imported picture.
  }
  \item{prefix}{
    A character string. A prefix to add to referenced gridSVG content
    (e.g. pattern fills). Only used when \code{gridSVG} is
    \code{TRUE}. The reference label must be a unique reference label,
    otherwise an error will result. This can be checked by calling
    gridSVG's \code{listSVGDefinitions()}. When this parameter is
    \code{NULL}, a prefix will automatically be generated but this is
    not guaranteed to be unique.
  }
  \item{\dots}{
    For \code{grid.symbols()}, arguments to be passed onto
    \code{symbolsGrob()}. For \code{symbolsGrob()}, additional
    parameters to be passed onto the \code{picture}'s \code{grobify}
    method.
  }
  \item{name}{
    A character identifier.
  }
}
\value{
  A grid grob.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{grid.picture}}
}
