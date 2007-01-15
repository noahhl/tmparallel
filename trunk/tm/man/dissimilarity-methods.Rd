\name{dissimilarity-methods}
\docType{methods}
\alias{dissimilarity}
\alias{dissimilarity-methods}
\alias{dissimilarity,TermDocMatrix,ANY,character-method}
\alias{dissimilarity,TextDocument,TextDocument,character-method}
\title{Methods for Function dissimilarity in Package `tm'}
\description{
Methods for function \code{dissimilarity} in package \pkg{tm}.
}
\section{Methods}{
\describe{

\item{x = "TermDocMatrix", y = "ANY", method = "character"}{Computes the
  dissimilarity between the documents in \code{x}
  with the distance measure \code{method}. Available methods are:
  \itemize{
    \item \code{cosine} Cosine Measure
    \item \code{pearson} Pearson Measure
    \item \code{extjacc} Extended Jaccard Measure
  }
  Formally the dissimilarity is returned in form of a \code{dist}
  object.
}

\item{x = "TextDocument", y = "TextDocument", method =
  "character"}{Computes the dissimilarity between \code{x} and \code{y}
  with the distance measure \code{method}. Available methods are:
  \itemize{
    \item \code{cosine} Cosine Measure
    \item \code{pearson} Pearson Measure
    \item \code{extjacc} Extended Jaccard Measure
  }}
}}
\keyword{methods}
