# Author: Ingo Feinerer

# S4 class definition
# Text document
setClass("textdocument",
         representation(author = "character",
                        timestamp = "character",
                        description = "character",
                        id = "integer",
                        origin = "character",
                        heading = "character"),
         contains = c("character"))

# Accessor functions as described in "S4 Classes in 15 pages, more or less"

if (!isGeneric("author")) {
    if (is.function("author"))
        fun <- author
    else fun <- function(object) standardGeneric("author")
    setGeneric("author", fun)
}
setMethod("author", "textdocument", function(object) object@author)

if (!isGeneric("timestamp")) {
    if (is.function("timestamp"))
        fun <- timestamp
    else fun <- function(object) standardGeneric("timestamp")
    setGeneric("timestamp", fun)
}
setMethod("timestamp", "textdocument", function(object) object@timestamp)

if (!isGeneric("description")) {
    if (is.function("description"))
        fun <- description
    else fun <- function(object) standardGeneric("description")
    setGeneric("description", fun)
}
setMethod("description", "textdocument", function(object) object@description)

if (!isGeneric("id")) {
    if (is.function("id"))
        fun <- id
    else fun <- function(object) standardGeneric("id")
    setGeneric("id", fun)
}
setMethod("id", "textdocument", function(object) object@id)

if (!isGeneric("origin")) {
    if (is.function("origin"))
        fun <- origin
    else fun <- function(object) standardGeneric("origin")
    setGeneric("origin", fun)
}
setMethod("origin", "textdocument", function(object) object@origin)

if (!isGeneric("heading")) {
    if (is.function("heading"))
        fun <- heading
    else fun <- function(object) standardGeneric("heading")
    setGeneric("heading", fun)
}
setMethod("heading", "textdocument", function(object) object@heading)
