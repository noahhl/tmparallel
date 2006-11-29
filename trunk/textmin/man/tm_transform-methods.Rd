\name{tm_transform-methods}
\docType{methods}
\alias{tm_transform}
\alias{tm_transform-methods}
\alias{tm_transform,TextDocCol,function-method}
\title{Methods for Function tm\_transform in Package `textmin'}
\description{
Methods for function \code{tm_transform} in Package `textmin'
}
\section{Methods}{
\describe{

\item{object = "TextDocCol", FUN}{Applies \code{FUN} onto each element
  of \code{object}. Note that global metadata is automatically passed to
  \code{FUN} as argument \code{GlobalMetaData}. \code{FUN} must be a
  function which returns a \code{TextDocument}.}
}}
\keyword{methods}
