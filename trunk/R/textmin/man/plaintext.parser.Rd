\name{plaintext.parser}
\alias{plaintext.parser}
\title{Parse Text Document}
\description{
  Parse a text document without knowledge about internal structure and
  possible available metadata.
}
\usage{
plaintext.parser(file, lod)
}
\arguments{
  \item{file}{a character string giving the name of the file to parse.}
  \item{lod}{a logical indicating whether the text corpus should be
    loaded immediately into memory.}
}
\value{
  A \code{PlainTextDocument} representing \code{file}.
}
\author{Ingo Feinerer}
\keyword{file}
