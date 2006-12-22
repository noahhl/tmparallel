\name{convert_mbox_eml}
\alias{convert_mbox_eml}
\title{Convert E-Mails From mbox Format To eml Format}
\description{
  Convert e-mails from mbox (i.e., several mails in a single box) format
  to eml (i.e., every mail in a single file) format.
}
\usage{
convert_mbox_eml(mbox, eml.dir)
}
\arguments{
  \item{mbox}{a character describing the mbox location.}
  \item{eml.dir}{a character describing the output directory.}
}
\value{
  No explicit return value. As a side product the directory
  \code{eml.dir} contains the e-mails.
}
\author{Ingo Feinerer}
\keyword{file}
