\name{toPlainTextDocument-methods}
\docType{methods}
\alias{toPlainTextDocument}
\alias{toPlainTextDocument-methods}
\alias{toPlainTextDocument,PlainTextDocument,ANY-method}
\alias{toPlainTextDocument,XMLTextDocument,function-method}
\title{Methods for Function toPlainTextDocument in Package `textmin'}
\description{
Methods for function \code{toPlainTextDocument} in Package `textmin'
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
  \code{\link{reuters21578xml_to_plain}}
  \code{\link{rcv1_to_plain}}
}
\keyword{methods}
