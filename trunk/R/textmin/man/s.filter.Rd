\name{s.filter}
\alias{s.filter}
\title{Statement Filter}
\description{
  Filter metadata by special statements.
}
\usage{
s.filter(object, s, ..., GlobalMetaData)
}
\arguments{
  \item{object}{a \code{TextDocument} to be the filter applied to.}
  \item{s}{a statement of format \code{list(tag1 = "regexpr1", tag2 =
      "regexpr2", ...)}.}
  \item{...}{arguments passed over by calling functions.}
  \item{GlobalMetaData}{A named list, i.e. tag-value pairs, representing
    global available metadata.}
}
\details{
  The statement \code{s} models a simple query language. It consists of
  tag-value pairs (i.e., a named list). Tags in \code{s} represent
  metadata variables. If name conflicts arise, they are checked in the
  order local metadata, global metadata and finally object slots. Each
  value is a regular expression, which is applied on the object's
  metadata specified by the corresponding tag.

  The tag-value statements in \code{s} form a logical conjunction, thus
  to return \code{TRUE} all requirements must be matched.
  
  If \code{s} matches \code{list()}, i.e., is empty, no requirements
  need to be checked and \code{TRUE} is returned.
}
\value{
  A Boolean value indicating whether \code{object} fulfils all
  requirements described by \code{s}.
}
\author{Ingo Feinerer}
\keyword{file}
