\name{readReut21578XML}
\alias{readReut21578XML}
\alias{readReut21578XMLasPlain}
\title{Read In a Reuters-21578 XML Document}
\description{
  Read in a Reuters-21578 \acronym{XML} document.
}
\usage{
readReut21578XML(elem, language, id)
readReut21578XMLasPlain(elem, language, id)
}
\arguments{
  \item{elem}{A \code{list} with the named element \code{content} which
    must hold the document to be read in.}
  \item{language}{A \code{character} vector giving the text's language.}
  \item{id}{A \code{character} vector representing a unique identification
    string for the returned text document.}
}
\value{
   A \code{Reuters21578Document} for \code{readReut21578XML}, or a
   \code{PlainTextDocument} for \code{readReut21578XMLasPlain}.
}
\references{
  Lewis, David (1997) \emph{Reuters-21578 Text Categorization Collection Distribution 1.0}.
  \url{http://kdd.ics.uci.edu/databases/reuters21578/reuters21578.html}

  Luz, Saturnino \emph{\acronym{XML}-encoded version of Reuters-21578}.
  \url{http://modnlp.berlios.de/reuters21578.html}
}
\seealso{
  \code{\link{getReaders}} to list available reader functions.
}
\author{Ingo Feinerer}
