\name{convert_rcv1_plain}
\alias{convert_rcv1_plain}
\title{Transform A RCV1 Document To A Plain Text Document}
\description{
  Transform a Reuters Corpus Volume 1 XML document to a plain text
  document.
}
\usage{
convert_rcv1_plain(node, ...)
}
\arguments{
  \item{node}{an XML node representing a <newsitem></newsitem> element
    from a well-formed RCV1 XML file.}
  \item{...}{Arguments passed over by calling functions.}
}
\value{
  A \code{PlainTextDocument} representing \code{node}.
}
\seealso{\code{\link{as.plain}}}
\author{Ingo Feinerer}
\keyword{file}
