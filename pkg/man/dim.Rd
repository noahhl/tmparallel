\name{dim-methods}
\docType{methods}
\alias{dim-methods}
\alias{dim,TermDocumentMatrix-method}
\title{Methods for Function dim in Package `tm'}
\description{
Methods for function \code{dim} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{x = "TermDocumentMatrix"}{Retrieve the dimension of the
      term-document matrix.}
  }
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)
dim(tdm)
}
\keyword{methods}
