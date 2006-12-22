\name{remove_meta-methods}
\docType{methods}
\alias{remove_meta}
\alias{remove_meta-methods}
\alias{remove_meta,TextDocCol-method}
\alias{remove_meta,TextRepository-method}
\title{Methods for Function attach\_metadata in Package `textmin'}
\description{
Methods for function \code{remove_meta} in Package `textmin'
}
\section{Methods}{
  \describe{
    \item{object = "TextDocCol", dcmeta = NULL, dmeta = NULL}{Returns a text
      document collection where the \code{dcmeta} is removed from the
      \code{DCMetaData} and \code{dmeta} is removed from the \code{DMetaData}.}  
    \item{object = "TextRepository", dcmeta = NULL, dmeta = NULL}{Returns a text
      repository where \code{dcmeta} is removed from the metadata slot of the
      \code{object} text repository.}
  }
}
\seealso{
  \code{\link{DMetaData}}
  \code{\link{DCMetaData}}
  \code{\link{RepoMetaData}}
}
\keyword{methods}
