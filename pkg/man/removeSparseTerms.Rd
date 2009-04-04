\name{removeSparseTerms-methods}
\docType{methods}
\alias{removeSparseTerms}
\alias{removeSparseTerms-methods}
\alias{removeSparseTerms,TermDocumentMatrix,numeric-method}
\title{Methods for Function removeSparseTerms in Package `tm'}
\description{
Methods for function \code{removeSparseTerms} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "TermDocumentMatrix", sparse = "numeric"}{Return
      \code{object} where those terms are removed which have at least a
      \code{sparse} percentage of empty (i.e., terms occurring 0 times
      in a document) elements. I.e., the resulting matrix contains only
      terms with a sparse factor of less than \code{sparse}.
    }
  }
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)
removeSparseTerms(tdm, 0.2)
}
\keyword{methods}
