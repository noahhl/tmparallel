\name{reuters21578xml.parser}
\alias{reuters21578xml.parser}
\title{Parse Reuters21578 XML Document}
\description{
  Parse a Reuters21578 XML document.
}
\usage{
reuters21578xml.parser(file, lod)
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
