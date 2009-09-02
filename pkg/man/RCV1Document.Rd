\name{RCV1Document}
\alias{RCV1Document}
\title{RCV1 Text Document}
\description{
  Construct an object representing a RCV1 XML text document with additional
  information. The XML document itself is represented by inheriting from
  \code{XMLTextDocument}.
}
\usage{
RCVDocument(x, author = character(0), datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"), description = character(0), heading = character(0), id = character(0), origin = character(0), language = character(0), localmetadata = list())
}
\seealso{
  \code{\link{PlainTextDocument}} and \code{\link{Reuters21578Document}}
}
\author{Ingo Feinerer}
