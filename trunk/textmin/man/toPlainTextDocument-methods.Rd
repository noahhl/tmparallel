\name{as.plaintext_doc-methods}
\docType{methods}
\alias{as.plaintext_doc}
\alias{as.plaintext_doc-methods}
\alias{as.plaintext_doc,PlainTextDocument,ANY-method}
\alias{as.plaintext_doc,XMLTextDocument,function-method}
\title{Methods for Function as.plaintext\_doc in Package `textmin'}
\description{
Methods for function \code{as.plaintext_doc} in Package `textmin'
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
  \code{\link{reut21578xml_to_plain}}
  \code{\link{rcv1_to_plain}}
}
\keyword{methods}
