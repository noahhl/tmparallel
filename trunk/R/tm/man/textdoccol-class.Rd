\name{textdoccol-class}
\docType{class}
\alias{textdoccol-class}
\alias{tdm}
\alias{tdm,textdoccol-method}
\title{Text document collection}
\description{
  A class representing a collection of text documents.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("textdoccol",...)} or by calling the function
  \code{textdoccol}.
}
\section{Slots}{
  \describe{
    \item{\code{tdm}:}{Object of class \code{"termdocmatrix"} containing
      the term-document matrix for the text document collection. A column
      represents a term, and a row the id of a text document, respectively.}
  }
}
\section{Extends}{
Class \code{"list"}, directly.
}
\section{Methods}{
  \describe{
    \item{tdm}{\code{signature(object = "textdoccol")}: returns the
      term-document matrix}
  }
}
\author{Ingo Feinerer \email{h0125130@wu-wien.ac.at}}
\seealso{
  \code{\link{textdoccol}}
}
\keyword{classes}
