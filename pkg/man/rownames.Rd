\name{rownames-methods}
\docType{methods}
\alias{rownames-methods}
\alias{rownames,TermDocumentMatrix-method}
\title{Methods for Function rownames in Package `tm'}
\description{
Methods for function \code{rownames} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{x = "TermDocumentMatrix"}{Retrieve the row names of the
      term-document matrix \code{x}.}
  }
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)
rownames(tdm)
}
\keyword{methods}
