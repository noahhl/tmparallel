# Author: Ingo Feinerer
# S4 class and accessor definitions
# Accessor functions are implemented as described in "S4 Classes in 15 pages, more or less"

# Text document
setClass("textdocument",
         representation(author = "character",
                        datetimestamp = "character",
                        description = "character",
                        id = "integer",
                        origin = "character",
                        heading = "character"),
         contains = c("character"))

if (!isGeneric("author")) {
    if (is.function("author"))
        fun <- author
    else
        fun <- function(object) standardGeneric("author")
    setGeneric("author", fun)
}
setMethod("author", "textdocument", function(object) object@author)

if (!isGeneric("datetimestamp")) {
    if (is.function("datetimestamp"))
        fun <- timestamp
    else
        fun <- function(object) standardGeneric("datetimestamp")
    setGeneric("datetimestamp", fun)
}
setMethod("datetimestamp", "textdocument", function(object) object@timestamp)

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


# Text document collection
setClass("textdoccol",
         contains = c("list"))


# Term-document matrix
setClass("termdocmatrix",
         representation(weighting = "character"),
         contains = c("matrix"))

if (!isGeneric("weighting")) {
    if (is.function("weighting"))
        fun <- weighting
    else
        fun <- function(object) standardGeneric("weighting")
    setGeneric("weighting", fun)
}
setMethod("weighting", "termdocmatrix", function(object) object@weighting)
