\name{ncol-methods}
\docType{methods}
\alias{ncol-methods}
\alias{ncol,TermDocumentMatrix-method}
\title{Methods for Function ncol in Package `tm'}
\description{
Methods for function \code{ncol} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{x = "TermDocumentMatrix"}{Retrieve the number of columns of the
      term-document matrix.}
  }
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)
ncol(tdm)
}
\keyword{methods}
