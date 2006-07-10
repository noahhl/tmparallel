\name{termdocmatrix}
\docType{methods}
\alias{termdocmatrix}
\alias{termdocmatrix,textdoccol-method}
\title{Term-document matrix}
\description{
  Constructs a term-document matrix.
}
\usage{
\S4method{termdocmatrix}{textdoccol}(object, weighting = "tf", stemming
= FALSE, language = "english", minWordLength = 3, minDocFreq = 1,
stopwords = NULL)
}
\arguments{
  \item{object}{a text document collection}
  \item{weighting}{the weighting mode for the term-document
    matrix. Possible settings are
    \itemize{
      \item \code{tf} Term frequency
      \item \code{tf-idf} Term frequency inverse document frequency
      \item \code{bin} Binary frequency
    }
  }
  \item{stemming}{if set, stems words before making the term-document matrix.}
  \item{language}{the language determines the stemming rules}
  \item{minWordLength}{words smaller than this number are discarded for
    the term-document matrix.}
  \item{minDocFreq}{words that appear less often in documents than this
    number are discarded for the term-document matrix.}
  \item{stopwords}{a plain text file with all stopwords}
}
\value{
  An S4 object of class \code{termdocmatrix} which extends the class
  \code{matrix} containing a term-document matrix. The following slots
  contain useful information:

  \item{weighting}{The weighting mode applied to the term-document matrix}
}
\author{Ingo Feinerer \email{h0125130@wu-wien.ac.at}}
\keyword{methods}
