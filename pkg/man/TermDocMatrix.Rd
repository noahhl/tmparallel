\name{TermDocumentMatrix}
\docType{methods}
\alias{TermDocMatrix}
\alias{TermDocumentMatrix}
\alias{DocumentTermMatrix}
\alias{TermDocumentMatrix,Corpus-method}
\alias{as.matrix,TermDocumentMatrix-method}
\title{Term-Document Matrix}
\description{
  Constructs a term-document matrix or a document-term matrix.
}
\usage{
TermDocumentMatrix(object, control = list())

}
\arguments{
  \item{object}{a corpus}
  \item{control}{a named list of control options. The component
    \code{weighting} must be a weighting function capable of handling a
    \code{\linkS4class{TermDocumentMatrix}}. It defaults to \code{weightTf} for term frequency
    weighting. All other options are delegated internally to a
    \code{\link{termFreq}} call.}
}
\value{
  An S4 object of class \code{\linkS4class{TermDocumentMatrix}} containing a sparse term-document
  matrix or document-term matrix. The following slots contain useful information:

  \item{Transpose}{Indicates whether the current instantiation is a
  term-document matrix (\code{FALSE}) or a document-term matrix (\code{TRUE}).}
  \item{Weighting}{The weighting applied to the matrix.}
}
\seealso{
  The documentation of \code{\link{termFreq}} gives an extensive list of
  possible options.

  Available weighting functions shipped with the \pkg{tm}
  package are \code{\link{weightTf}}, \code{\link{weightTfIdf}}, and
  \code{\link{weightBin}}.
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude, control = list(weighting = weightTfIdf, stopwords = TRUE))
dtm <- DocumentTermMatrix(crude, control = list(weighting = weightTfIdf, stopwords = TRUE))
summary(tdm[155:160,1:5])
summary(dtm[1:5,155:160])
}
\author{Ingo Feinerer}
