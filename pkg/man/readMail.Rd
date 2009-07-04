\name{readMail}
\alias{readMail}
\title{Read In an E-Mail Document}
\description{
  Returns a function which reads in an electronic mail document.
}
\usage{
readMail(DateFormat = "\%d \%B \%Y \%H:\%M:\%S", ...)
}
\arguments{
  \item{DateFormat}{the format of the Date header in the mail document.}
  \item{\dots}{arguments for the generator function.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which in turn reads in a mail document) with a well-defined
  signature, but can access passed over arguments (e.g., to specify the
  format of the Date header in the e-mail via \code{DateFormat}) via
  lexical scoping.
}
\value{
  A \code{function} with the signature \code{elem, language, id}:
  
  \item{elem}{A \code{list} with the two named elements \code{content}
    and \code{uri}. The first element must hold the document to
    be read in, the second element must hold a call to extract this
    document. The call is evaluated upon a request for load on demand.}
  \item{language}{A \code{character} vector giving the text's language.}
  \item{id}{A \code{character} vector representing a unique identification
    string for the returned text document.}

  The function returns a \code{MailDocument} representing
  \code{content}.
}
\seealso{
  Use \code{\link{getReaders}} to list available reader functions.

  See \code{\link{strptime}} for date format specifications.
}
\author{Ingo Feinerer}
