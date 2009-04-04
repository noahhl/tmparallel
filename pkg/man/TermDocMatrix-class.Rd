\name{TermDocumentMatrix-class}
\docType{class}
\alias{TermDocumentMatrix-class}
\alias{Weighting}
\alias{Weighting,TermDocumentMatrix-method}
\title{Term-Document Matrix}
\description{
  A class representing either a sparse term-document matrix or a sparse
  document-term matrix. For performance reasons both matrix types are
  internally stored in the same format: a row represents a term, and a
  column represents (the ID of) a document, respectively. Only a flag
  (the slot \code{Transpose}) identifies the current instantiation.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("TermDocumentMatrix",...)} or by calling the function
  \code{TermDocumentMatrix}.
}
\section{Slots}{
  \describe{
    \item{\code{Transpose}:}{a \code{logical} indicating whether the
      current instantiated class is a term-document matrix
      (\code{FALSE}) or a document-term matrix (\code{TRUE}).}
    \item{\code{Weighting}:}{a \code{character} containing
      the weighting which was applied to the matrix. Implemented are term
      frequency, term frequency-inverse document frequency, and binary.}
  }
}
\section{Methods}{
  \describe{
    \item{Weighting}{\code{signature(object = "TermDocumentMatrix")}: Returns the
      weighting mode.}
  }
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{TermDocumentMatrix}}
}
\keyword{classes}
