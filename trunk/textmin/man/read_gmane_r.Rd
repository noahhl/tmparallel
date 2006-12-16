\name{read_gmane_r}
\alias{read_gmane_r}
\title{Read In A Newsgroup Document}
\description{
  Returns a function which reads in a RSS feed as returned by Gmane for
  the R mailing lists.
}
\usage{
read_gmane_r(...)
}
\arguments{
  \item{...}{arguments for the generator function.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a RSS feed) with
  a well-defined
  signature, but can access passed over arguments via lexical
  scoping. This is especially useful for reader functions for complex
  data structures which need a lot of configuration options.
}
\value{
  A \code{function} with the signature \code{elem, lodsupport, load,
    id}.

  \itemize{
    \item{\code{elem}:} A \code{list} with the two named elements
    \code{content} and \code{uri}. The first element must hold the
    document corpus to be read in, the second element must hold a
    call to the document corpus. The call is evaluated upon
    a request for load on demand.
    \item{\code{lodsupport}:} A \code{logical} value indicating whether
    Load On Demand is supported by the input device.
    \item{\code{load}:} A \code{logical} value indicating whether the
    document corpus should be immediately loaded into memory.
    \item{\code{id}:} A \code{character} representing a unique
    identification string for the returned text document.
  }

  The function returns a \code{NewsgroupDocument} representing
  \code{content}.
}
\author{Ingo Feinerer}
\keyword{file}
