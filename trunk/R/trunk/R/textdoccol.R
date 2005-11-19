# Author: Ingo Feinerer

# S4 class definition
# Text document collection
setClass("textdoccol",
         representation(tdm = "termdocmatrix"),
         contains = c("list"))

# Accessor functions as described in "S4 Classes in 15 pages, more or less"

if (!isGeneric("tdm")) {
    if (is.function("tdm"))
        fun <- tdm
    else
        fun <- function(object) standardGeneric("tdm")
    setGeneric("tdm", fun)
}
setMethod("tdm", "textdoccol", function(object) object@tdm)

# Constructors

setGeneric("textdoccol", function(object, ...) standardGeneric("textdoccol"))
# Read in text documents in XML Reuters Corpus Volume 1 (RCV1) format
setMethod("textdoccol",
          c("character", "logical", "logical",  "character", "logical", "character", "integer", "integer", "logical"),
          function(object, stripWhiteSpace = FALSE, toLower = FALSE, weighting = "tf", stemming = FALSE,
                   language = "german", minWordLength = 3, minDocFreq = 1, stopwords = NULL) {
              require(XML)

              tree <- xmlTreeParse(object)
              tdcl <- new("textdoccol", .Data = xmlApply(xmlRoot(tree), parseNewsItem, stripWhiteSpace, toLower))
              tdcl@tdm = termdocmatrix(tdcl, weighting, stemming, language, minWordLength, minDocFreq, stopwords)

              tdcl
          })

# TODO: Implement lacking fields.
# For this we need the full RCV1 XML set to know where to find those things
parseNewsItem <- function(node, stripWhiteSpace = FALSE, toLower = FALSE) {
    author <- "Not yet implemented"
    timestamp <- xmlAttrs(node)[["date"]]
    description <- "Not yet implemented"
    id <- as.integer(xmlAttrs(node)[["itemid"]])
    origin <- "Not yet implemented"
    corpus <- unlist(xmlApply(node[["text"]], xmlValue), use.names = FALSE)

    if (stripWhiteSpace)
        corpus <- gsub("[[:space:]]+", " ", corpus)
    if (toLower)
        corpus <- tolower(corpus)

    heading <- xmlValue(node[["title"]])

    new("textdocument", .Data = corpus, author = author, timestamp = timestamp,
        description = description, id = id, origin = origin, heading = heading)
}
