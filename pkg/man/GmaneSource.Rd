\name{GmaneSource}
\docType{methods}
\alias{GmaneSource}
\title{Gmane Source}
\description{
  Constructs a source for a Gmane mailing list RSS feed.
}
\usage{
GmaneSource(x, encoding = "UTF-8")
}
\arguments{
  \item{x}{Either a character identifying the file or a connection.}
  \item{encoding}{A character giving the encoding of the file.}
}
\value{
  An object of class \code{GmaneSource} which extends the class
  \code{Source} representing a Gmane mailing list RSS feed.
}
\author{Ingo Feinerer}
