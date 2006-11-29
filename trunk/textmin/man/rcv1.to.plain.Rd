\name{rcv1_to_plain}
\alias{rcv1_to_plain}
\title{Transform RCV1 Document To Plain Text Document}
\description{
  Transform a Reuters Corpus Volume 1 XML document to a plain text
  document.
}
\usage{
rcv1_to_plain(node, ...)
}
\arguments{
  \item{node}{an XML node representing a <newsitem></newsitem> element
    from a well-formed RCV1 XML file.}
  \item{...}{Arguments passed over by calling functions.}
}
\value{
  A \code{PlainTextDocument} representing \code{node}.
}
\seealso{\code{\link{as.plaintext_doc}}}
\author{Ingo Feinerer}
\keyword{file}
