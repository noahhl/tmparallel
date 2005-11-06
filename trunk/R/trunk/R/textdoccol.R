# Author: Ingo Feinerer

# S4 class definition
# Text document collection
# TODO: Define proper S4 term-document matrix
setClass("textdoccol", representation(docs = "list",
                                      tdm = "matrix"))

# Accessor function
if (!isGeneric("docs")) {
    if (is.function("docs"))
        fun <- docs
    else
        fun <- function(object) standardGeneric("docs")
    setGeneric("docs", fun)
}
setMethod("docs", "textdoccol", function(object) object@docs)

setGeneric("textdoccol", function(object) standardGeneric("textdoccol"))
# Read in text documents in XML Reuters Corpus Volume 1 (RCV1) format
setMethod("textdoccol", "character", function(object) {
    require(XML)

    tree <- xmlTreeParse(object)
    new("textdoccol", docs = xmlApply(xmlRoot(tree), parseNewsItem), tdm = matrix())
})

# TODO: Implement lacking fields.
# For this we need the full RCV1 XML set to know where to find those things
parseNewsItem <- function(node) {
    author <- "Not yet implemented"
    timestamp <- xmlAttrs(node)[["date"]]
    description <- "Not yet implemented"
    id <- as.integer(xmlAttrs(node)[["itemid"]])
    origin <- "Not yet implemented"
    # TODO: Concatenate list elements (= XML paragraphs) to a single string
    corpus <- unlist(xmlApply(node[["text"]], xmlValue), use.names = FALSE)
    heading <- xmlValue(node[["title"]])

    new("textdocument", author = author, timestamp = timestamp, description = description,
        id = id, origin = origin, corpus = corpus, heading = heading)
}
