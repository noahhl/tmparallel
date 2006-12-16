\name{tm_index-methods}
\docType{methods}
\alias{tm_index}
\alias{tm_index-methods}
\alias{tm_index,TextDocCol-method}
\title{Methods for Function tm\_index in Package `textmin'}
\description{
Methods for function \code{tm_index} in Package `textmin'
}
\section{Methods}{
  \describe{
    \item{object = "TextDocCol", FUN  = s\_filter, doclevel = FALSE}{If
      \code{doclevel} is set, the method applies \code{FUN} onto each element
      of \code{object} and returns the indices; otherwise
      \code{FUN} is applied to \code{object} itself. Note that metadata is
      automatically passed to \code{FUN}. The function \code{FUN} must
      return a Boolean value.}
  }
}
\seealso{
  \code{\link{s_filter}}
  \code{\link{tm_filter}}
}
\keyword{methods}
