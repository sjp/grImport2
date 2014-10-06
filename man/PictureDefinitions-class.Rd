\name{PictureDefinitions-class}
\docType{class}
\alias{PictureDefinitions-class}
\alias{getDef}
\alias{getDef-methods}
\alias{setDef}
\alias{setDef-methods}
\alias{getDef,PictureDefinitions,character-method}
\alias{setDef,PictureDefinitions,character,PictureContent-method}

\title{Class \code{"PictureDefinitions"}}
\description{
  A collection of graphical content and features that are to be
  referenced by the rest of the image. These definitions are not
  intended to draw anything themselves, but describe how other pieces of
  graphical content will be drawn (or in the case of clipping paths, not
  drawn).
}
\section{Slots}{
  \describe{
    \item{\code{content}:}{
      Object of class \code{"list"}. A list of \code{"PictureContent"}
      objects.
    }
  }
}
\section{Methods}{
  \describe{
    \item{getDef}{
      \code{signature(object = "PictureDefinitions", id =
        "character")}: retrieves an object that is referred to by a
      given label.
    }
    \item{setDef}{
      \code{signature(object = "PictureDefinitions", id = "character",
      value = "PictureContent")}: Not intended to be used by regular
      users, but this is a method for assigning a definition to be
      referenced later by \code{"PictureContent"} objects. This method
      stores a new definition (\code{value}) with a given label
      (\code{id}). The result is that a new \code{"PictureDefinitions"}
      object is returned with the new definition added to it.
    }
  }
}
\author{
  Simon Potter
}
\seealso{
  \code{\linkS4class{Picture}}
}
