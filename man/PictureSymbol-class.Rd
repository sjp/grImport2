\name{PictureSymbol-class}
\docType{class}
\alias{PictureSymbol-class}

\title{Class \code{"PictureSymbol"}}
\description{
  A description of a symbol that is meant to be used (perhaps
  repeatedly) as part of a larger picture (as it does not directly draw
  anything itself). This object only has an effect while parsing a
  picture (via \code{\link{readPicture}}), so is not intended to be used
  directly by any user.
}
\section{Slots}{
  \describe{
    \item{\code{definition}:}{
      Object of class \code{"list"}. A list of \code{"PictureContent"}
      objects.
    }
  }
}
\section{Extends}{
  Class \code{\linkS4class{PictureContent}}, directly.
}
\author{
  Simon Potter
}
