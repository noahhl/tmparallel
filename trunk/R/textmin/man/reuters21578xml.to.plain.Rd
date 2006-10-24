\name{reuters21578xml.to.plain}
\alias{reuters21578xml.to.plain}
\title{Transform Reuters21578 XML Document To Plain Text Document}
\description{
  Transform a Reuters21578 XML document to a plain text
  document.
}
\usage{
reuters21578xml.to.plain(node)
}
\arguments{
  \item{node}{an XML node representing a <REUTERS></REUTERS> element
    from a well-formed Reuters-21578 XML file.}
}
\value{
  A \code{PlainTextDocument} representing \code{node}.
}
\seealso{\code{\link{toPlainTextDocument}}}
\author{Ingo Feinerer}
\keyword{file}
