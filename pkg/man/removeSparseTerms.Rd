\name{removeSparseTerms}
\alias{removeSparseTerms}
\title{Remove Sparse Terms from a Term-Document Matrix}
\description{
  Remove sparse terms from a term-document matrix.
}
\usage{
removeSparseTerms(x, sparse)
}
\arguments{
  \item{x}{A term-document matrix.}
  \item{sparse}{A numeric for the maximal allowed sparsity.}
}
\value{
  A term-document matrix where those terms from \code{x} are
  removed which have at least a \code{sparse} percentage of empty (i.e.,
  terms occurring 0 times in a document) elements. I.e., the resulting
  matrix contains only terms with a sparse factor of less than
  \code{sparse}.
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)
removeSparseTerms(tdm, 0.2)
}
