\name{termdocmatrix-class}
\docType{class}
\alias{termdocmatrix-class}
\alias{weighting}
\alias{weighting,termdocmatrix-method}
\title{Term-document matrix}
\description{
  A class representing a term-document matrix. A column represents a
  term, and a row the id of a text document, respectively.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("termdocmatrix",...)} or by calling the function
  \code{termdocmatrix}.
}
\section{Slots}{
  \describe{
    \item{\code{weighting}:}{Object of class \code{"character"} containing
      the mode which was applied on the matrix. Possible are term
      frequency \code{"tf"}, term frequency-inverse document frequency
      \code{"tf-idf"} and binary \code{"bin"}.}
  }
}
\section{Extends}{
Class \code{"matrix"}, directly.
}
\section{Methods}{
  \describe{
    \item{weighting}{\code{signature(object = "termdocmatrix")}: returns the
      weithing mode}
  }
}
\author{Ingo Feinerer \email{h0125130@wu-wien.ac.at}}
\seealso{
  \code{\link{termdocmatrix}}
}
\keyword{classes}
