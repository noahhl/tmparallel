# Author: Ingo Feinerer

# S4 class definition
# Text document
setClass("textdocument", representation(author = "character",
                                        timestamp = "character",
                                        description = "character",
                                        id = "integer",
                                        origin = "character",
                                        corpus = "character",
                                        heading = "character"))

# Accessor function
if (!isGeneric("author")) {
    if (is.function("author"))
        fun <- author
    else fun <- function(object) standardGeneric("author")
    setGeneric("author", fun)
}
setMethod("author", "textdocument", function(object) object@author)

# Accessor function
if (!isGeneric("timestamp")) {
    if (is.function("timestamp"))
        fun <- timestamp
    else fun <- function(object) standardGeneric("timestamp")
    setGeneric("timestamp", fun)
}
setMethod("timestamp", "textdocument", function(object) object@timestamp)

# Accessor function
if (!isGeneric("description")) {
    if (is.function("description"))
        fun <- description
    else fun <- function(object) standardGeneric("description")
    setGeneric("description", fun)
}
setMethod("description", "textdocument", function(object) object@description)

# Accessor function
if (!isGeneric("id")) {
    if (is.function("id"))
        fun <- id
    else fun <- function(object) standardGeneric("id")
    setGeneric("id", fun)
}
setMethod("id", "textdocument", function(object) object@id)

# Accessor function
if (!isGeneric("origin")) {
    if (is.function("origin"))
        fun <- origin
    else fun <- function(object) standardGeneric("origin")
    setGeneric("origin", fun)
}
setMethod("origin", "textdocument", function(object) object@origin)

# Accessor function
if (!isGeneric("corpus")) {
    if (is.function("corpus"))
        fun <- corpus
    else fun <- function(object) standardGeneric("corpus")
    setGeneric("corpus", fun)
}
setMethod("corpus", "textdocument", function(object) object@corpus)

# Accessor function
if (!isGeneric("heading")) {
    if (is.function("heading"))
        fun <- heading
    else fun <- function(object) standardGeneric("heading")
    setGeneric("heading", fun)
}
setMethod("heading", "textdocument", function(object) object@heading)
