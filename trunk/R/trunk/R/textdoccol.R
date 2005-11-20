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
              type <- match.arg(inputType,c("RCV1","CSV","REUT21578"))
              switch(type,
                     # Read in text documents in XML Reuters Corpus Volume 1 (RCV1) format
                     # For the moment the first argument is still a single file
                     # This will be changed to a directory as soon as we have the full RCV1 data set
                     "RCV1" = {
                         require(XML)

                         tree <- xmlTreeParse(object)
                         tdcl <- new("textdoccol", .Data = xmlApply(xmlRoot(tree), parseNewsItem, stripWhiteSpace, toLower))
                     },
                     # Text in a special CSV format (as e.g. exported from an Excel sheet)
                     # For details on the file format see data/Umfrage.csv
                     # The first argument has to be a single file
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
                     },
                     # Read in text documents in Reuters-21578 XML (not SGML) format
                     # Typically the first argument will be a directory where we can
                     # find the files reut2-000.xml ... reut2-021.xml
                     "REUT21578" = {
                         require(XML)

                         tdl <- sapply(dir(object,
                                           pattern = ".xml",
                                           full.names = TRUE),
                                       function(file) {
                                           tree <- xmlTreeParse(file)
                                           xmlApply(xmlRoot(tree), parseReuters, stripWhiteSpace, toLower)
                                       })

                         tdcl <- new("textdoccol", .Data = tdl)
                     })

              tdcl@tdm <- termdocmatrix(tdcl, weighting, stemming, language, minWordLength, minDocFreq, stopwords)

              tdcl
          })

# Parse a <newsitem></newsitem> element from a well-formed RCV1 XML file
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

# Parse a <REUTERS></REUTERS> element from a well-formed Reuters-21578 XML file
parseReuters <- function(node, stripWhiteSpace = FALSE, toLower = FALSE) {
    author <- "Not yet implemented"
    timestamp <- xmlValue(node[["DATE"]])
    description <- "Not yet implemented"
    id <- as.integer(xmlAttrs(node)[["NEWID"]])

    origin <- "Not yet implemented"

    # The <BODY></BODY> tag is unfortunately NOT obligatory!
    if (!is.null(node[["TEXT"]][["BODY"]]))
        corpus <- xmlValue(node[["TEXT"]][["BODY"]])
    else
        corpus <- ""

    if (stripWhiteSpace)
        corpus <- gsub("[[:space:]]+", " ", corpus)
    if (toLower)
        corpus <- tolower(corpus)

    # The <TITLE></TITLE> tag is unfortunately NOT obligatory!
    if (!is.null(node[["TEXT"]][["TITLE"]]))
        heading <- xmlValue(node[["TEXT"]][["TITLE"]])
    else
        heading <- ""

    new("textdocument", .Data = corpus, author = author, timestamp = timestamp,
        description = description, id = id, origin = origin, heading = heading)
}
