\name{reut21578xml_to_plain}
\alias{reut21578xml_to_plain}
\title{Transform Reuters21578 XML Document To Plain Text Document}
\description{
  Transform a Reuters21578 XML document to a plain text
  document.
}
\usage{
reut21578xml_to_plain(node, ...)
}
\arguments{
  \item{node}{an XML node representing a <REUTERS></REUTERS> element
    from a well-formed Reuters-21578 XML file.}
  \item{...}{Arguments passed over by calling functions.}
}
\value{
  A \code{PlainTextDocument} representing \code{node}.
}
\seealso{\code{\link{as.plaintext_doc}}}
\author{Ingo Feinerer}
\keyword{file}
