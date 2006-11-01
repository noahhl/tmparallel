\name{find_assocs-methods}
\docType{methods}
\alias{find_assocs}
\alias{find_assocs-methods}
\alias{find_assocs,TermDocMatrix,character-method}
\alias{find_assocs,matrix,character-method}
\title{Methods for Function find\_assocs in Package `textmin'}
\description{
Methods for function \code{find_assocs} in Package `textmin'
}
\section{Methods}{
\describe{

\item{object = "TermDocMatrix", term = "character", corlimit}{finds those terms
  from \code{object} which correlate with \code{term} more than \code{corlimit}}

\item{object = "matrix", term = "character", corlimit}{finds those terms
  from the correlation matrix \code{object} which correlate with \code{term} more than \code{corlimit}}
}}
\keyword{methods}
