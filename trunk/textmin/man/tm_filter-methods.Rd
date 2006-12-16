\name{tm_filter-methods}
\docType{methods}
\alias{tm_filter}
\alias{tm_filter-methods}
\alias{tm_filter,TextDocCol-method}
\title{Methods for Function tm\_filter in Package `textmin'}
\description{
Methods for function \code{tm_filter} in Package `textmin'
}
\section{Methods}{
  \describe{
    \item{object = "TextDocCol", FUN  = s\_filter, doclevel = FALSE}{If
      \code{doclevel} is set, the method applies \code{FUN} onto each element
      of \code{object} and returns the filtered \code{object}; otherwise
      \code{FUN} is applied to \code{object} itself, i.e., the
      filter is applied onto the \code{object}. Note that metadata is
      automatically passed to \code{FUN}. The function \code{FUN} must
      return a Boolean value.}
  }
}
\seealso{
  \code{\link{s_filter}}
  \code{\link{tm_index}}
}
\keyword{methods}
