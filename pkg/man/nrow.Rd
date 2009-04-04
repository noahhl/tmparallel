\name{nrow-methods}
\docType{methods}
\alias{nrow-methods}
\alias{nrow,TermDocumentMatrix-method}
\title{Methods for Function nrow in Package `tm'}
\description{
Methods for function \code{nrow} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{x = "TermDocumentMatrix"}{Retrieve the number of rows of the
      term-document matrix.}
  }
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)
nrow(tdm)
}
\keyword{methods}
