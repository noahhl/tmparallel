\name{uci.kdd.newsgroup.parser}
\alias{uci.kdd.newsgroup.parser}
\title{Parse UCI KDD Newsgroup Dataset Document}
\description{
  Parse a UCI KDD Newsgroup Dataset document. Thus each newsgroup mail
  must be named after a unique \code{ID} and contain distinct meta
  information in form of headers.
}
\usage{
uci.kdd.newsgroup.parser(file, lod)
}
\arguments{
  \item{file}{a character string giving the name of the file to parse.}
  \item{lod}{a logical indicating whether the text corpus should be
    loaded immediately into memory.}
}
\value{
  A \code{NewsgroupDocument} representing \code{file}.
}
\author{Ingo Feinerer}
\keyword{file}
