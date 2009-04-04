\name{dimnames-methods}
\docType{methods}
\alias{dimnames-methods}
\alias{dimnames,TermDocumentMatrix-method}
\title{Methods for Function dimnames in Package `tm'}
\description{
Methods for function \code{dimnames} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{x = "TermDocumentMatrix"}{Retrieve the dimnames of the
      term-document matrix.}
  }
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)
dimnames(tdm)
}
\keyword{methods}
