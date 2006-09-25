\name{TextRepository-class}
\docType{class}
\alias{TextRepository-class}
\alias{RepresentationMetaData}
\alias{RepresentationMetaData,TextRepository-method}
\title{Text document collection}
\description{
  A class representing a repository of text document collections.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("TextRepository",...)} or by calling the function
  \code{TextRepository}.
}
\section{Slots}{
  \describe{
    \item{\code{RepresentationMetaData}:}{Object of class \code{"list"} containing
      representational metadata for the text document collections in form of
      tag-value pairs.}
  }
}
\section{Extends}{
Class \code{"list"}, directly.
}
\section{Methods}{
  \describe{
    \item{RepresentationMetaData}{\code{signature(object = "TextRepository")}: returns
      representational metadata in form of a tag-value paired list.}
  }
}
\author{Ingo Feinerer \email{h0125130@wu-wien.ac.at}}
\seealso{
  \code{\link{TextRepository}}
}
\keyword{classes}
