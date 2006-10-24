\name{rcv1.parser}
\alias{rcv1.parser}
\title{Parse Reuters Corpus Volume 1 XML Document}
\description{
  Parse a Reuters Corpus Volume 1 XML document.
}
\usage{
rcv1.parser(file, lod)
}
\arguments{
  \item{file}{a character string giving the name of the file to parse.}
  \item{lod}{a logical indicating whether the text corpus should be
    loaded immediately into memory.}
}
\value{
  An \code{XMLTextDocument} representing \code{file}.
}
\author{Ingo Feinerer}
\keyword{file}
