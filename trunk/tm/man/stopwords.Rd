\name{stopwords}
\alias{stopwords}
\title{Multilingual Stopwords}
\description{
  Return stopwords in different languages.
}
\usage{
stopwords(language = "en")
}
\arguments{
  \item{language}{a \code{character} giving the desired language.}
}
\details{At the moment supported languages are \code{english},
  \code{french}, and \code{german}.}
\value{A character vector containing the requested stopwords.}
\examples{
stopwords("english")
}
\keyword{file}
