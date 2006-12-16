\name{as.plain-methods}
\docType{methods}
\alias{as.plain}
\alias{as.plain-methods}
\alias{as.plain,PlainTextDocument,ANY-method}
\alias{as.plain,XMLTextDocument,function-method}
\title{Methods for Function as.plaintext\_doc in Package `textmin'}
\description{
Methods for function \code{as.plain} in Package `textmin'
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument"}{Returns \code{object}.}
    \item{object = "XMLTextDocument", FUN}{Converts \code{object} to a
      \code{PlainTextDocument} by applying \code{FUN} on the XML root node
      and returns the new object.}
  }
}
\seealso{
  \code{\link{convert_reut21578xml_plain}}
  \code{\link{convert_rcv1_plain}}
}
\keyword{methods}
