\name{rcv1_to_plain}
\alias{rcv1_to_plain}
\title{Transform RCV1 Document To Plain Text Document}
\description{
  Transform a Reuters Corpus Volume 1 XML document to a plain text
  document.
}
\usage{
rcv1_to_plain(node)
}
\arguments{
  \item{node}{an XML node representing a <newsitem></newsitem> element
    from a well-formed RCV1 XML file.}
}
\value{
  A \code{PlainTextDocument} representing \code{node}.
}
\seealso{\code{\link{toPlainTextDocument}}}
\author{Ingo Feinerer}
\keyword{file}
