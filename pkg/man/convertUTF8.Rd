\name{convert_UTF_8}
\alias{convert_UTF_8}
\title{Convert Document Encoding to UTF-8}
\description{
  Converts the encoding of a text document to UTF-8.
}
\usage{
convert_UTF_8(x, from = "", sub = NA)
}
\arguments{
  \item{x}{A text document.}
  \item{from}{A character string describing the current encoding.}
  \item{sub}{A character string.  If not 'NA' it is used to replace any
    non-convertible bytes in the input.}
}
\value{
  The text document \code{x} converted to UTF-8 encoding.
}
\details{
  This function internally uses \code{\link{iconv}} for conversion, and
  the arguments \code{from} and \code{sub} are passed over.
}
\seealso{
  \code{\link{iconv}}
}
\examples{
data("crude")
convert_UTF_8(crude[[1]])
}
\author{Ingo Feinerer}
