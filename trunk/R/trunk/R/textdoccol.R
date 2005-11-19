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
setMethod("textdoccol",
          c("character", "character", "logical", "logical",  "character",
            "logical", "character", "integer", "integer", "logical"),
          function(object, inputType = "RCV1", stripWhiteSpace = FALSE, toLower = FALSE, weighting = "tf",
                   stemming = FALSE, language = "english", minWordLength = 3, minDocFreq = 1, stopwords = NULL) {

              # Add a new type for each unique input source format
              type <- match.arg(inputType,c("RCV1","CSV"))
              switch(type,
                     # Read in text documents in XML Reuters Corpus Volume 1 (RCV1) format
                     "RCV1" = {
                         require(XML)

                         tree <- xmlTreeParse(object)
                         tdcl <- new("textdoccol", .Data = xmlApply(xmlRoot(tree), parseNewsItem, stripWhiteSpace, toLower))
                     },
                     # Text in CSV format (as e.g. exported from an Excel sheet)
                     "CSV" = {
                         m <- as.matrix(read.csv(object))
                         l <- vector("list", dim(m)[1])
                         for (i in 1:dim(m)[1]) {
                             author <- "Not yet implemented"
                             timestamp <- date()
                             description <- "Not yet implemented"
                             id <- i
                             corpus <- as.character(m[i,2:dim(m)[2]])
                             if (stripWhiteSpace)
                                 corpus <- gsub("[[:space:]]+", " ", corpus)
                             if (toLower)
                                 corpus <- tolower(corpus)
                             origin <- "Not yet implemented"
                             heading <- "Not yet implemented"

                             l[[i]] <- new("textdocument", .Data = corpus, author = author, timestamp = timestamp,
                                 description = description, id = id, origin = origin, heading = heading)
                         }
                         tdcl <- new("textdoccol", .Data = l)
                     }
                     )

              tdcl@tdm <- termdocmatrix(tdcl, weighting, stemming, language, minWordLength, minDocFreq, stopwords)

              tdcl
          })

# Parse a <newsitem></newsitem> element from a valid RCV1 XML file
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
