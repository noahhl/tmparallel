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

# Read in XML text documents
# Reuters Corpus Volume 1 (RCV1)
readXML <- function(file) {
    tree <- xmlTreeParse(file)
    root <- xmlRoot(tree)

    # TODO: At each loop node points to the current newsitem
    node <- root

    # TODO: Implement lacking fields.
    # For this we need the full RCV1 XML set to know where to find those things
    author <- "Not yet implemented"
    date <- xmlAttrs(node)[["date"]]
    description <- "Not yet implemented"
    id <- as.integer(xmlAttrs(node)[["itemid"]])
    origin <- "Not yet implemented"
    text <- xmlSApply(node[["text"]], xmlValue)
    title <- xmlValue(node[["title"]])

    doc <- new("textdocument", author = author, date = date, description = description,
               id = id, origin = origin, text = text, title = title)

    new("textdoccol", docs = list(doc), matrix = ())
}

setGeneric("textdoccol", function(object) standardGeneric("textdoccol"))
setMethod("textdoccol", "file", readXML)
