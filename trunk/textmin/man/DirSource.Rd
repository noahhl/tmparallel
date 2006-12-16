\name{DirSource}
\docType{methods}
\alias{DirSource}
\alias{DirSource,character-method}
\title{Directory Source}
\description{
  Constructs a directory source.
}
\usage{
\S4method{DirSource}{character}(directory, load = FALSE, recursive = FALSE)
}
\arguments{
  \item{directory}{a directory.}
  \item{load}{a logical value indicating whether the text corpus should
    be loaded immediately into memory (\code{load = TRUE}) or loaded when
    necessary (\code{load = FALSE}). This allows to minimize memory
    demands for large document collections.}
  \item{recursive}{a logical value indicating whether the directory
    should be traversed recursively.}
}
\value{
  An S4 object of class \code{DirSource} which extends the class
  \code{Source} representing a directory.
}
\author{Ingo Feinerer}
\keyword{methods}
