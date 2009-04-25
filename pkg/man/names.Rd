\name{names}
\alias{rownames.TermDocumentMatrix}
\alias{colnames.TermDocumentMatrix}
\alias{dimnames.TermDocumentMatrix}
\alias{Docs}
\alias{Terms}
\title{Row, Column, Dim Names, Document IDs, and Terms}
\description{
  Retrieve the row names, column names, dimnames, document IDs, and
  terms of a term-document matrix.
}
\arguments{
  \item{x}{A term-document matrix.}
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)[1:10,1:20]
rownames(tdm)
colnames(tdm)
dimnames(tdm)
Docs(tdm)
Terms(tdm)
}
