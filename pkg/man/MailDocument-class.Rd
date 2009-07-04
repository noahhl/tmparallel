\name{MailDocument-class}
\docType{class}
\alias{MailDocument-class}
\title{E-Mail Document}
\description{
  A class representing an electronic mail document.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MailDocument", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{character} containing the mail
      content.}
    \item{\code{Author}:}{Object of class \code{character} containing
      the author names.}
    \item{\code{DateTimeStamp}:}{Object of class \code{character} containing the
      date and time when the mail was written/sent.}
    \item{\code{Description}:}{Object of class \code{character} containing
      additional text information.}
    \item{\code{ID}:}{Object of class \code{integer} containing
      an identifier (if present in the mail header the message ID).}
    \item{\code{Origin}:}{Object of class \code{character} containing
      information on the source and origin of the text (e.g., when
      posted in a newsgroup).}
    \item{\code{Heading}:}{Object of class \code{character} containing
      the mail subject.}
    \item{\code{Language}:}{Object of class \code{character} containing
      the language of the text.}
    \item{\code{LocalMetaData}:}{Object of class \code{list} containing
      the local meta data in form of tag-value pairs.}
    \item{\code{Header}:}{Object of class \code{character} containing
      the mail header.}
  }
}
\section{Extends}{
Class \code{\linkS4class{PlainTextDocument}}, directly.
}
\author{Ingo Feinerer}
\keyword{classes}
