\name{PictureFeColorMatrix-class}
\docType{class}
\alias{PictureFeColorMatrix-class}
\alias{grobify,PictureFeColorMatrix-method}

\title{Class \code{"PictureFeColorMatrix"}}
\description{
  A description of a colour transformation matrix filter effect
  primitive. To be used in conjunction with the gridSVG package.
}
\section{Slots}{
  \describe{
    \item{\code{type}:}{Object of class \code{"character"} ~~ }
    \item{\code{input}:}{Object of class \code{"character"} ~~ }
    \item{\code{values}:}{Object of class \code{"matrix"} ~~ }
  }
}
\section{Extends}{
  Class \code{\linkS4class{PictureContent}}, directly.
}
\section{Methods}{
  \describe{
    \item{grobify}{
      \code{signature(object = "PictureFeColorMatrix")}: creates an
      \code{feColorMatrix} object to include in a filter effect object
      for use with the gridSVG package.
    }
  }
}
\author{
  Simon Potter
}
\seealso{
  \code{\linkS4class{PictureFilter}}
}
