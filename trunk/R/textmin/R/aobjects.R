# Author: Ingo Feinerer
# S4 class and accessor definitions
# Assignment and accessor functions are implemented as described in "S4 Classes in 15 pages, more or less"

# Text document
setClass("TextDocument",
         representation(Author = "character",
                        DateTimeStamp = "character",
                        Description = "character",
                        ID = "character",
                        Origin = "character",
                        Heading = "character",
                        LocalMetaData = "list"))

if (!isGeneric("Author")) {
    if (is.function("Author"))
        fun <- Author
    else
        fun <- function(object) standardGeneric("Author")
    setGeneric("Author", fun)
}
setMethod("Author", "TextDocument", function(object) object@Author)
setGeneric("Author<-", function(x, value) standardGeneric("Author<-"))
setReplaceMethod("Author", "TextDocument", function(x, value) {
  x@Author <- value
  x
})

if (!isGeneric("DateTimeStamp")) {
    if (is.function("DateTimeStamp"))
        fun <- DateTimeStamp
    else
        fun <- function(object) standardGeneric("DateTimeStamp")
    setGeneric("DateTimeStamp", fun)
}
setMethod("DateTimeStamp", "TextDocument", function(object) object@DateTimeStamp)
setGeneric("DateTimeStamp<-", function(x, value) standardGeneric("DateTimeStamp<-"))
setReplaceMethod("DateTimeStamp", "TextDocument", function(x, value) {
  x@DateTimeStamp <- value
  x
})

if (!isGeneric("Description")) {
    if (is.function("Description"))
        fun <- Description
    else fun <- function(object) standardGeneric("Description")
    setGeneric("Description", fun)
}
setMethod("Description", "TextDocument", function(object) object@Description)
setGeneric("Description<-", function(x, value) standardGeneric("Description<-"))
setReplaceMethod("Description", "TextDocument", function(x, value) {
  x@Description <- value
  x
})

if (!isGeneric("ID")) {
    if (is.function("ID"))
        fun <- ID
    else fun <- function(object) standardGeneric("ID")
    setGeneric("ID", fun)
}
setMethod("ID", "TextDocument", function(object) object@ID)
setGeneric("ID<-", function(x, value) standardGeneric("ID<-"))
setReplaceMethod("ID", "TextDocument", function(x, value) {
  x@ID <- value
  x
})

if (!isGeneric("Origin")) {
    if (is.function("Origin"))
        fun <- Origin
    else fun <- function(object) standardGeneric("Origin")
    setGeneric("Origin", fun)
}
setMethod("Origin", "TextDocument", function(object) object@Origin)
setGeneric("Origin<-", function(x, value) standardGeneric("Origin<-"))
setReplaceMethod("Origin", "TextDocument", function(x, value) {
  x@Origin <- value
  x
})

if (!isGeneric("Heading")) {
    if (is.function("Heading"))
        fun <- Heading
    else fun <- function(object) standardGeneric("Heading")
    setGeneric("Heading", fun)
}
setMethod("Heading", "TextDocument", function(object) object@Heading)
setGeneric("Heading<-", function(x, value) standardGeneric("Heading<-"))
setReplaceMethod("Heading", "TextDocument", function(x, value) {
  x@Heading <- value
  x
})

if (!isGeneric("LocalMetaData")) {
    if (is.function("LocalMetaData"))
        fun <- LocalMetaData
    else fun <- function(object) standardGeneric("LocalMetaData")
    setGeneric("LocalMetaData", fun)
}
setMethod("LocalMetaData", "TextDocument", function(object) object@LocalMetaData)

# Inherited text documents
# Plain text documents
setClass("PlainTextDocument",
         representation(FileName = "character", Cached = "logical"),
         contains = c("character", "TextDocument"))

if (!isGeneric("Corpus")) {
    if (is.function("Corpus"))
        fun <- Corpus
    else
        fun <- function(object) standardGeneric("Corpus")
    setGeneric("Corpus", fun)
}
setMethod("Corpus", "PlainTextDocument", function(object) object@.Data)
setGeneric("Corpus<-", function(x, value) standardGeneric("Corpus<-"))
setReplaceMethod("Corpus", "PlainTextDocument", function(x, value) {
  x@.Data <- value
  x
})

if (!isGeneric("FileName")) {
    if (is.function("FileName"))
        fun <- FileName
    else fun <- function(object) standardGeneric("FileName")
    setGeneric("FileName", fun)
}
setMethod("FileName", "PlainTextDocument", function(object) object@FileName)

if (!isGeneric("Cached")) {
    if (is.function("Cached"))
        fun <- Cached
    else fun <- function(object) standardGeneric("Cached")
    setGeneric("Cached", fun)
}
setMethod("Cached", "PlainTextDocument", function(object) object@Cached)
setGeneric("Cached<-", function(x, value) standardGeneric("Cached<-"))
setReplaceMethod("Cached", "PlainTextDocument", function(x, value) {
  x@Cached <- value
  x
})

# XML text document
# If XMLDocument would be a S4 class, we could directly inherit from it
# Instead we have to do a work-around with a list
setClass("XMLTextDocument",
         representation(FileName = "character", Cached = "logical"),
         contains = c("list", "TextDocument"))

setMethod("Corpus", "XMLTextDocument", function(object) object@.Data)
setReplaceMethod("Corpus", "XMLTextDocument", function(x, value) {
    x@.Data <- value
    x
})
setMethod("FileName", "XMLTextDocument", function(object) object@FileName)
setMethod("Cached", "XMLTextDocument", function(object) object@Cached)
setReplaceMethod("Cached", "XMLTextDocument", function(x, value) {
    x@Cached <- value
    x
})

# Newsgroup document as found in the Newsgroup dataset of the UCI KDD archive
setClass("NewsgroupDocument",
         representation(Newsgroup = "character", FileName = "character", Cached = "logical"),
         contains = c("character", "TextDocument"))

setMethod("Corpus", "NewsgroupDocument", function(object) object@.Data)
setReplaceMethod("Corpus", "NewsgroupDocument", function(x, value) {
    x@.Data <- value
    x
})
setMethod("FileName", "NewsgroupDocument", function(object) object@FileName)
setMethod("Cached", "NewsgroupDocument", function(object) object@Cached)
setReplaceMethod("Cached", "NewsgroupDocument", function(x, value) {
  x@Cached <- value
  x
})

# Text document collection
setClass("TextDocCol",
         representation(GlobalMetaData = "list"),
         contains = c("list"))

if (!isGeneric("GlobalMetaData")) {
    if (is.function("GlobalMetaData"))
        fun <- GlobalMetaData
    else fun <- function(object) standardGeneric("GlobalMetaData")
    setGeneric("GlobalMetaData", fun)
}
setMethod("GlobalMetaData", "TextDocCol", function(object) object@GlobalMetaData)

# Repository for text document collections
setClass("TextRepository",
         representation(RepresentationMetaData = "list"),
         contains = c("list"))

if (!isGeneric("RepresentationMetaData")) {
    if (is.function("RepresentationMetaData"))
        fun <- RepresentationMetaData
    else fun <- function(object) standardGeneric("RepresentationMetaData")
    setGeneric("RepresentationMetaData", fun)
}
setMethod("RepresentationMetaData", "TextRepository", function(object) object@RepresentationMetaData)

# Term-document matrix
setClass("TermDocMatrix",
         representation(Weighting = "character"),
         contains = c("matrix"))

if (!isGeneric("Weighting")) {
    if (is.function("Weighting"))
        fun <- Weighting
    else
        fun <- function(object) standardGeneric("Weighting")
    setGeneric("Weighting", fun)
}
setMethod("Weighting", "TermDocMatrix", function(object) object@Weighting)
setGeneric("Weighting<-", function(x, value) standardGeneric("Weighting<-"))
setReplaceMethod("Weighting", "TermDocMatrix", function(x, value) {
  x@Weighting <- value
  x
})

# Source objects

# Mimic S4 class until their official definition is found in R
setOldClass("file", prototype = file())

setClass("Source",
         representation(LoDSupport = "logical",
                        Position = "numeric"))

# A directory with files
setClass("DirSource",
         representation(FileList = "character",
                        Load = "logical"),
         contains = c("Source"))

# A single CSV file where each line is interpreted as document
setClass("CSVSource",
         representation(FileName = "character",
                        Content = "character"),
         contains = c("Source"))

# A single XML file consisting of several Reuters21578 documents
# This format can be directly used for the reut2-???.xml files
# from the official Reuters21578 XML archive
setClass("Reuters21578XMLSource",
         representation(FileName = "character",
                        Content = "list"),
         contains = c("Source"))
