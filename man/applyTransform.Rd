\name{applyTransform}
\alias{applyTransform}
\title{
  Transform a Picture Object by a Transformation Matrix
}
\description{
  Transforms a \code{"Picture"} object (or indeed \code{"PictureContent"}
  objects) using a 3x3 transformation matrix.
}
\usage{
applyTransform(object, tm)
}
\arguments{
  \item{object}{
    Either a \code{"Picture"} object or a \code{"PictureContent"} object.
  }
  \item{tm}{
    A 3x3 numeric transformation matrix.
  }
}
\value{
  The transformed object.
}
\author{
  Simon Potter
}
