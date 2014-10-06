\name{applyTransform-gpar}
\docType{methods}
\alias{applyTransform,gpar,matrix-method}
\title{ Transform a grid \code{gpar} object }
\description{
  Transform a grid \code{gpar} object.
}
\section{Methods}{
  \describe{
    \item{
      \code{signature(object = "gpar", tm = "matrix")}}{ Transforms the
      \code{"lwd"} and \code{"lty"} components of a \code{gpar} object
      (if present) by a 3x3 transformation matrix and returns the
      modified gpar object.
    }
  }
}
\author{
  Simon Potter
}
