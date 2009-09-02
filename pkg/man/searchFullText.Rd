\name{searchFullText}
\alias{searchFullText}
\alias{searchFullText.PlainTextDocument}
\title{Full Text Search}
\description{
  Perform a full text search in text documents.
}
\usage{
\method{searchFullText}{PlainTextDocument}(x, pattern)
}
\value{
  \code{TRUE} if the regular expression \code{pattern} matches in
  \code{x}, otherwise \code{FALSE}.
}
\seealso{
  Use \code{\link{getFilters}} to list available filter functions.
}
\examples{
data("crude")
searchFullText(crude[[1]], "co[m]?pany")
}
