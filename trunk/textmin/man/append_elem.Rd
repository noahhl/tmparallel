\name{append_elem-methods}
\docType{methods}
\alias{append_elem}
\alias{append_elem-methods}
\alias{append_elem,TextDocCol,TextDocument-method}
\alias{append_elem,TextRepository,TextDocCol-method}
\title{Methods for Function append\_elem in Package `textmin'}
\description{
Methods for function \code{append_elem} in Package `textmin'
}
\section{Methods}{
  \describe{
    \item{object = "TextDocCol", data = "TextDocument", meta = NULL}{Returns a text
      document collection where the \code{data} text document is added to
      the data slot of the \code{object} text document
      collection. Optionally meta data (\code{DMetaData}) can be added with
      \code{meta} to the document collection.}
    
    \item{object = "TextRepository", data = "TextDocCol", meta = NULL}{Returns a text
      repository where the \code{data} text document collection is added to
      the data slot of the \code{object} text repository. Optionally meta
      data can be added with \code{meta} to the repository.}
  }
}
\seealso{
\code{\link{DMetaData}}
\code{\link{RepoMetaData}}
}
\keyword{methods}
