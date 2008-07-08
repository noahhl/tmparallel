\name{Dictionary}
\docType{methods}
\alias{Dictionary}
\alias{Dictionary,character-method}
\alias{Dictionary,TermDocMatrix-method}
\title{Dictionary}
\description{
  Constructs a directory from a character vector or a term-document matrix.
}
\usage{
Dictionary(object)
}
\arguments{
  \item{object}{A character vector or a term-document matrix holding the terms for the dictionary.}
}
\value{
  An S4 object of class \code{Dictionary} which extends the class
  \code{character} representing a dictionary.
}
\author{Ingo Feinerer}
\keyword{methods}
