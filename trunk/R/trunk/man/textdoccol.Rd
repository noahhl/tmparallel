\name{textdoccol}
\docType{methods}
\alias{textdoccol}
\alias{textdoccol,character-method}
\title{Text document collection}
\description{
  Constructs a text document collection from various sources.
}
\usage{
\S4method{textdoccol}{character}(object, inputType = "CSV",
stripWhiteSpace = FALSE, toLower = FALSE)
}
\arguments{
  \item{object}{a directory containing the text documents.}
  \item{inputType}{determines the input format. Possible settings are
    \itemize{
      \item \code{CSV} Comma seperated values format

      This input format requires no special markup (like XML). Instead a
      plain file with comma separated values per line is used. The first
      column has to be the identification number for the text document,
      the rest is used as the text corpus. All lines must have the same
      number of arguments.

      This format is especially useful for the fast import from other
      sources (like export from Excel files, \ldots).
      \item \code{RCV1} Reuters Corpus Volume 1 XML format

      At the moment this format is not fully supported. It will be
      implemented as soon I have access to the full sources (they are
      not freely available in the net).
      \item \code{REUT21578} Reuters 21578 XML format

      A file format as used by the Reuters21578 XML format. See there
      for details.

      TODO: Insert link to the Reuters data set in XML format.
      \item \code{RIS} Austrian Rechtsinformationssystem des Bundes HTML
      format.

      Each HTML file has to follow this name schema:
      \code{\emph{Geschäftszahl}.html}. This ensures that we can
      identify each document, as the body of the HTML file is not
      further investigated.

      Austrian RIS: \url{http://ris.bka.gv.at/}
    }
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
