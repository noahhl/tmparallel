\name{SCorpus-class}
\docType{class}
\alias{SCorpus-class}
\alias{CMetaData,SCorpus-method}
\alias{DMetaData,SCorpus-method}
\alias{DMetaData<-,SCorpus-method}
\title{Standard Corpus}
\description{
  A class representing a standard corpus.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("SCorpus",...)} or by calling the function
  \code{SCorpus}.
}
\section{Slots}{
  \describe{
    \item{\code{CMetaData}:}{Object of class \code{MetaDataNode} containing
      corpus specific meta data in form of tag-value pairs and information about
      children in form of a binary tree. This information is useful for
      reconstructing meta data after e.g. merging corpora.}
    \item{\code{DMetaData}:}{Object of class \code{data.frame} containing
      the document specific meta data for the collection. This data frame
      typically encompasses clustering or classification results which
      basically are metadata for documents but form an own entity (e.g.,
      with its name, the value range, etc.).}
  }
}
\section{Extends}{
Class \code{list}, directly.
}
\section{Methods}{
  \describe{
    \item{CMetaData}{\code{signature(object = "SCorpus")}: Returns the
      corpus specific metadata in form of a tag-value
      paired list.}
    \item{DMetaData}{\code{signature(object = "SCorpus")}: Returns the
      document specific metadata in form of a data frame.}
  }
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{MetaDataNode-class}}
  \code{\link{SCorpus}}
}
\keyword{classes}
