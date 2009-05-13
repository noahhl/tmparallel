\name{[-methods}
\docType{methods}
\alias{[,PCorpus-method}
\alias{[,SCorpus-method}
\alias{[<-,PCorpus-method}
\alias{[<-,SCorpus-method}
\alias{[[,PCorpus-method}
\alias{[[,SCorpus-method}
\alias{[[<-,PCorpus-method}
\alias{[[<-,SCorpus-method}
\title{Methods for Subset Functions in Package `tm'}
\description{
  Methods for subset functions \code{[}, \code{[<-}, \code{[[} and
  \code{[[<-} in package \pkg{tm}.
}
\arguments{
  \item{x = "SCorpus"}{The corpus to be subsetted
    or modified.}
  \item{x = "PCorpus"}{The corpus to be subsetted
    or modified.}
}
\examples{
data("crude")
summary(crude[10:15])
crude[[1]]
}
\keyword{methods}
