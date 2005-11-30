\name{textdocument-class}
\docType{class}
\alias{textdocument-class}
\alias{author}
\alias{timestamp}
\alias{description}
\alias{id}
\alias{origin}
\alias{heading}
\alias{author,textdocument-method}
\alias{timestamp,textdocument-method}
\alias{description,textdocument-method}
\alias{id,textdocument-method}
\alias{origin,textdocument-method}
\alias{heading,textdocument-method}
\title{Text document}
\description{
  A class representing a text document with additional information.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("textdocument", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{author}:}{Object of class \code{"character"} containing
      the author names}
    \item{\code{timestamp}:}{Object of class \code{"character"} containing the
      date and time when the document was written}
    \item{\code{description}:}{Object of class \code{"character"} containing
      additional text information}
    \item{\code{id}:}{Object of class \code{"integer"} containing
      an identifier}
    \item{\code{origin}:}{Object of class \code{"character"} containing
      information on the source and origin of the text}
    \item{\code{heading}:}{Object of class \code{"character"} containing
      the title or a short heading}
  }
}
\section{Extends}{
Class \code{"character"}, directly.
}
\section{Methods}{
  \describe{
    \item{author}{\code{signature(object = "textdocument")}: returns the
      author names}
    \item{timestamp}{\code{signature(object = "textdocument")}: returns the
      date and time when the document was written}
    \item{description}{\code{signature(object = "textdocument")}: returns
      additional text information}
    \item{id}{\code{signature(object = "textdocument")}: returns the
      identifier}
    \item{origin}{\code{signature(object = "textdocument")}: returns
      information on the source and origin of the text}
    \item{heading}{\code{signature(object = "textdocument")}: returns the
      the title or a short heading}
  }
}
\author{Ingo Feinerer \email{h0125130@wu-wien.ac.at}}
\keyword{classes}
