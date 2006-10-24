\name{TextDocCol-class}
\docType{class}
\alias{TextDocCol-class}
\alias{GlobalMetaData}
\alias{GlobalMetaData,TextDocCol-method}
\title{Text document collection}
\description{
  A class representing a collection of text documents.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("TextDocCol",...)} or by calling the function
  \code{TextDocCol}.
}
\section{Slots}{
  \describe{
    \item{\code{GlobalMetaData}:}{Object of class \code{"list"} containing
      the global metadata for the text document collection in form of
      tag-value pairs.}
  }
}
\section{Extends}{
Class \code{"list"}, directly.
}
\section{Methods}{
  \describe{
    \item{GlobalMetaData}{\code{signature(object = "TextDocCol")}: returns the
      global metadata in form of a tag-value paired list.}
  }
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{TextDocCol}}
}
\keyword{classes}
