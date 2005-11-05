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

setGeneric("textdoccol", function(docs) standardGeneric("textdoccol"))
# Read in XML text documents
# Reuters Corpus Volume 1 (RCV1)
setMethod("textdoccol", "character", function(docs) {
    require(XML)

    tree <- xmlTreeParse(docs)
    root <- xmlRoot(tree)

    # TODO: At each loop node points to the current newsitem
    node <- root

    # TODO: Implement lacking fields.
    # For this we need the full RCV1 XML set to know where to find those things
    author <- "Not yet implemented"
    timestamp <- xmlAttrs(node)[["date"]]
    description <- "Not yet implemented"
    id <- as.integer(xmlAttrs(node)[["itemid"]])
    origin <- "Not yet implemented"
    corpus <- unlist(xmlApply(node[["text"]], xmlValue), use.names = FALSE)

    heading <- xmlValue(node[["title"]])

    doc <- new("textdocument", author = author, timestamp = timestamp, description = description,
               id = id, origin = origin, corpus = corpus, heading = heading)

    new("textdoccol", docs = list(doc), tdm = matrix())
})
