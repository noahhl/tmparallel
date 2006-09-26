\name{TextDocCol}
\docType{methods}
\alias{TextDocCol}
\alias{TextDocCol,character-method}
\title{Text document collection}
\description{
  Constructs a text document collection from various sources.
}
\usage{
\S4method{TextDocCol}{character}(object, inputType = "CSV",
stripWhiteSpace = FALSE, toLower = FALSE)
}
\arguments{
  \item{object}{a directory containing the documents.}
  \item{inputType}{determines the input format. Possible settings are
    \itemize{
      \item \code{PLAIN} Plain text format

      Each entry in the \code{object} directory is imported without
      further knowledge about the file format. That means the file is
      interpreted as plain text without any markup or meta
      information. The document \code{ID}s are generated automatically
      and can be found together with the filename in the plain text
      document's metadata.
      \item \code{CSV} Comma seperated values format

      This input format requires no special markup (like XML). Instead a
      plain file with comma separated values per line is used. The first
      column has to be the identification number for the text document,
      the rest is used as the text corpus. All lines must have the same
      number of arguments.

      This format is especially useful for the fast import from other
      sources (like export from Excel files, \ldots).
      \item \code{RCV1} Reuters Corpus Volume 1 XML format

      The Reuters Corpus Volume 1 comes in a special XML format which
      can be imported directly with this switch.
      \item \code{REUT21578} Reuters 21578 XML format

      An XML file format as used by the Reuters21578 XML dataset. This
      switch returns directly plain text documents with the corpus in
      memory.
      \item \code{REUT21578_XML} Reuters 21578 XML format

      An XML file format as used by the Reuters21578 XML dataset. The
      pure \code{XMLTextDocument}s are returned with loading on demand
      activated. Thus the text corpus resides on disk until used.
      \item \code{NEWSGROUP} UCI KDD Newsgroup Dataset format.

      A file format as found in the UCI KDD Newsgroup dataset. Thus each
      newsgroup mail must be named after an unique \code{ID} and contain
      distinct meta information in form of headers.
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
  An S4 object of class \code{TextDocCol} which extends the class
  \code{list} containing a collection of text documents.
}
\author{Ingo Feinerer \email{h0125130@wu-wien.ac.at}}
\keyword{methods}
