\name{tm_tag_score}
\alias{tm_tag_score}
\alias{tm_tag_score.PlainTextDocument}
\alias{tm_tag_score.numeric}
\title{Compute a Tag Score}
\description{
  Compute a score based on the number of tags found in a document.
}
\usage{
\method{tm_tag_score}{PlainTextDocument}(x, tags, FUN = function(x) sum(x, na.rm = TRUE))
\method{tm_tag_score}{numeric}(x, tags, FUN)
}
\arguments{
  \item{x}{Either a plain text document or a named numeric as returned
    by \code{\link{termFreq}}.}
  \item{tags}{A character vector of tags to be matched.}
  \item{FUN}{A function computing a score from the number of tags
    matching in \code{x}.}
}
\value{
  A score as computed by \code{FUN} from the number of matching
  \code{tags} in \code{x}.
}
\examples{
data("acq")
tm_tag_score(acq[[1]], c("company", "change"))
}
