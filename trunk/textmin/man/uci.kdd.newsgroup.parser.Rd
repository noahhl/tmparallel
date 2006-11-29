\name{newsgroup_parser}
\alias{newsgroup_parser}
\title{Parse UCI KDD Newsgroup Dataset Document}
\description{
  Parse a \acronym{UCI} \acronym{KDD} Newsgroup Dataset document. Thus
  each newsgroup mail must be named after a unique \code{ID} and contain
  distinct meta information in form of headers.
}
\usage{
newsgroup_parser(...)
}
\arguments{
  \item{...}{arguments for generator function.}
}
\value{
  A \code{NewsgroupDocument} representing \code{file}.
}
\author{Ingo Feinerer}
\keyword{file}
