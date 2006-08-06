# Author: Ingo Feinerer
# S4 class and accessor definitions
# Accessor functions are implemented as described in "S4 Classes in 15 pages, more or less"

# Text document
setClass("TextDocument",
         representation(Author = "character",
                        DateTimeStamp = "character",
                        Description = "character",
                        ID = "integer",
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

if (!isGeneric("DateTimeStamp")) {
    if (is.function("DateTimeStamp"))
        fun <- DateTimeStamp
    else
        fun <- function(object) standardGeneric("DateTimeStamp")
    setGeneric("DateTimeStamp", fun)
}
setMethod("DateTimeStamp", "TextDocument", function(object) object@timestamp)

if (!isGeneric("Description")) {
    if (is.function("Description"))
        fun <- Description
    else fun <- function(object) standardGeneric("Description")
    setGeneric("Description", fun)
}
setMethod("Description", "TextDocument", function(object) object@Description)

if (!isGeneric("ID")) {
    if (is.function("ID"))
        fun <- ID
    else fun <- function(object) standardGeneric("ID")
    setGeneric("ID", fun)
}
setMethod("ID", "TextDocument", function(object) object@ID)

if (!isGeneric("Origin")) {
    if (is.function("Origin"))
        fun <- Origin
    else fun <- function(object) standardGeneric("Origin")
    setGeneric("Origin", fun)
}
setMethod("Origin", "TextDocument", function(object) object@Origin)

if (!isGeneric("Heading")) {
    if (is.function("Heading"))
        fun <- Heading
    else fun <- function(object) standardGeneric("Heading")
    setGeneric("Heading", fun)
}
setMethod("Heading", "TextDocument", function(object) object@Heading)

# Inherited text documents
# Plain text documents
setClass("PlainTextDocument",
         representation(FileName = "character", Cached = "numeric"),
         contains = c("character", "TextDocument"))
# XML text document
# If XMLDocument would be a S4 class, we could directly inherit from it
# Instead we have to do a work-around with a list
setClass("XMLTextDocument",
         representation(FileName = "character", Cached = "numeric"),
         contains = c("list", "TextDocument"))

# Text document collection
setClass("TextDocCol",
         representation(GlobalMetaData = "list"),
         contains = c("list"))

# Repository for text document collections
setClass("TextRepository",
         representation(RepresentationMetaData = "list"),
         contains = c("list"))

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
