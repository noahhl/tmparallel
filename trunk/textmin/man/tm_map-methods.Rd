\name{tm_map-methods}
\docType{methods}
\alias{tm_map}
\alias{tm_map-methods}
\alias{tm_map,TextDocCol,function-method}
\title{Methods for Function tm\_map in Package `textmin'}
\description{
Methods for function \code{tm_map} in Package `textmin'
}
\section{Methods}{
\describe{

\item{object = "TextDocCol", FUN}{Applies \code{FUN} onto each element
  of \code{object}. Note that metadata is automatically passed to
  \code{FUN}. \code{FUN} must be a function which returns a
  \code{TextDocument}.}
}}
\keyword{methods}
