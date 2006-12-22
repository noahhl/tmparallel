\name{GmaneRSource}
\docType{methods}
\alias{GmaneRSource}
\alias{GmaneRSource,character-method}
\alias{GmaneRSource,ANY-method}
\title{GmaneRectory Source}
\description{
  Constructs a source for a Gmane R mailing list RSS feed.
}
\usage{
\S4method{GmaneRSource}{character}(object)
}
\arguments{
  \item{object}{either a character identifying the file or a call which
    results in a connection.}
}
\value{
  An S4 object of class \code{GmaneRSource} which extends the class
  \code{Source} representing a Gmane R mailing list RSS feed.
}
\author{Ingo Feinerer}
\keyword{methods}
