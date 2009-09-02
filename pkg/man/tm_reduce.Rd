\name{tm_reduce}
\alias{tm_reduce}
\title{Combine Transformations}
\description{
Fold multiple transformations (mappings) into a single one.
}
\usage{
tmReduce(x, tmFuns, \dots)
}
\arguments{
  \item{x}{A corpus.}
  \item{tmFuns}{A \code{list} of \pkg{tm} transformations.}
  \item{\dots}{Arguments to the individual transformations.}
}
\value{
  A single \pkg{tm} transformation function.
}
\author{Ingo Feinerer}
\seealso{
  \code{Reduce}
  
  Use \code{\link{getTransformations}} to list available transformation (mapping) functions.
}
\examples{
data(crude)
crude[[1]]
skipWords <- function(x) removeWords(x, c("it", "the"))
funs <- list(tolower, removePunctuation, skipWords, stripWhitespace)
tm_map(crude, FUN = tm_reduce, tmFuns = funs)[[1]]
}
