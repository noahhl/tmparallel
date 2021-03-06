\name{DataframeSource}
\alias{DataframeSource}
\title{Data Frame Source}
\description{
  Constructs a source from a data frame.
}
\usage{
DataframeSource(x, encoding = "UTF-8")
}
\arguments{
  \item{x}{A data frame holding the texts.}
  \item{encoding}{A character giving the encoding of \code{x}.}
}
\value{
  An object of class \code{DataframeSource} which extends the class
  \code{Source} representing a data frame interpreting each row as a
  document.
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{getSources}} to list available sources.
}
\examples{
docs <- data.frame(docs = c("This is a text.", "This another one."),
                   row.names = c("Text 1", "Text 2"))
(ds <- DataframeSource(docs))
inspect(Corpus(ds))
}
