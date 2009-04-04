\name{dissimilarity-methods}
\docType{methods}
\alias{dissimilarity}
\alias{dissimilarity-methods}
\alias{dissimilarity,TermDocumentMatrix,ANY,character-method}
\alias{dissimilarity,TextDocument,TextDocument,character-method}
\title{Methods for Function dissimilarity in Package `tm'}
\description{
Methods for function \code{dissimilarity} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{x = "TermDocumentMatrix", y = "ANY", method = "character"}{Computes the
      dissimilarity between the documents in \code{x}
      with the distance measure \code{method}. Any method accepted by
      \code{dist} from package \pkg{proxy} can be passed over.

      Formally the dissimilarity is returned in form of a \code{dist}
      object.
    }

    \item{x = "TextDocument", y = "TextDocument", method =
      "character"}{Computes the dissimilarity between \code{x} and \code{y}
      with the distance measure \code{method}. Any method accepted by
      \code{dist} from package \pkg{proxy} can be passed over.}
  }
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)
dissimilarity(tdm, method = "cosine")
dissimilarity(crude[[1]], crude[[2]], method = "eJaccard")
}
\keyword{methods}
