\name{textdoccol}
\docType{methods}
\alias{textdoccol}
\alias{textdoccol,character-method}
\title{Text document collection}
\description{
  Constructs a text document collection.
}
\usage{
\S4method{textdoccol}{character}(object, inputType = "RCV1",
stripWhiteSpace = FALSE, toLower = FALSE)
}
\arguments{
  \item{object}{a file or directory containing the text documents.}
  \item{inputType}{determines the input format. Possible settings are
    \itemize{
      \item \code{RCV1} Reuters Corpus Volume 1 XML format
      \item \code{CSV} Comma seperated values format
      \item \code{REUT21578} Reuters 21578 XML format
    }
    TODO: Describe more details.
  }
  \item{stripWhiteSpace}{if set, replaces any white space in the texts with a
    single space.}
  \item{toLower}{if set, replaces all upper characters in the text with
    lower ones.}
}
\value{
  An S4 object of class \code{textdoccol} which extends the class
  \code{list} containing a collection of text documents.
}
\author{Ingo Feinerer \email{h0125130@wu-wien.ac.at}}
\keyword{methods}
