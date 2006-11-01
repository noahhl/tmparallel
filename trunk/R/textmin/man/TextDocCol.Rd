\name{TextDocCol}
\docType{methods}
\alias{TextDocCol}
\alias{TextDocCol,Source-method}
\title{Text document collection}
\description{
  Constructs a text document collection.
}
\usage{
\S4method{TextDocCol}{Source}(object, parser = plaintext.parser, lod = FALSE)
}
\arguments{
  \item{object}{a directory containing the documents.}
  \item{parser}{a parsing function capable of handling the file format
    found in \code{object}.}
  \item{lod}{a logical value indicating whether the text corpus should
    be loaded immediately into memory (\code{lod = TRUE}) or loaded when
    necessary (\code{lod = FALSE}). This allows to minimize memory
    demands for large document collections.}
}
\value{
  An S4 object of class \code{TextDocCol} which extends the class
  \code{list} containing a collection of text documents.
}
\author{Ingo Feinerer}
\keyword{methods}
