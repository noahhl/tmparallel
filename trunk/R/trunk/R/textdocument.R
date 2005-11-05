# Author: Ingo Feinerer

# S4 class definition
# Text document
setClass("textdocument", representation(author = "character",
                                        date = "character",
                                        description = "character",
                                        id = "integer",
                                        origin = "character",
                                        text = "character",
                                        title = "character"))

# Accessor function
if (!isGeneric("author")) {
    if (is.function("author"))
        fun <- author
    else fun <- function(object) standardGeneric("author")
    setGeneric("author", fun)
}
setMethod("author", "textdocument", function(object) object@author)

# Accessor function
if (!isGeneric("date")) {
    if (is.function("date"))
        fun <- date
    else fun <- function(object) standardGeneric("date")
    setGeneric("date", fun)
}
setMethod("date", "textdocument", function(object) object@date)

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
if (!isGeneric("text")) {
    if (is.function("text"))
        fun <- text
    else fun <- function(object) standardGeneric("text")
    setGeneric("text", fun)
}
setMethod("text", "textdocument", function(object) object@text)

# Accessor function
if (!isGeneric("title")) {
    if (is.function("title"))
        fun <- title
    else fun <- function(object) standardGeneric("title")
    setGeneric("title", fun)
}
setMethod("title", "textdocument", function(object) object@title)
