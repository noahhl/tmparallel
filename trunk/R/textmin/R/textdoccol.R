# Author: Ingo Feinerer

setGeneric("TextDocCol", function(object, inputType = "CSV", stripWhiteSpace = FALSE, toLower = FALSE) standardGeneric("TextDocCol"))
setMethod("TextDocCol",
          c("character"),
          function(object, inputType = "CSV", stripWhiteSpace = FALSE, toLower = FALSE) {
              # Add a new type for each unique input source format
              type <- match.arg(inputType,c("CSV", "RCV1", "REUT21578", "REUT21578_XML", "RIS"))
              switch(type,
                     # Text in a special CSV format
                     # For details on the file format see the R documentation file
                     # The first argument is a directory with .csv files
                     "CSV" = {
                         filelist <- dir(object, pattern = ".csv", full.names = TRUE)
                         tdl <- sapply(filelist,
                                       function(file) {
                                           m <- as.matrix(read.csv(file, header = FALSE))
                                           l <- vector("list", dim(m)[1])
                                           for (i in 1:dim(m)[1]) {
                                               author <- ""
                                               datetimestamp <- date()
                                               description <- ""
                                               id <- as.integer(m[i,1])
                                               corpus <- as.character(m[i,2:dim(m)[2]])
                                               if (stripWhiteSpace)
                                                   corpus <- gsub("[[:space:]]+", " ", corpus)
                                               if (toLower)
                                                   corpus <- tolower(corpus)
                                               origin <- "CSV"
                                               heading <- ""

                                               l[[i]] <- new("PlainTextDocument", .Data = corpus, Author = author, DateTimeStamp = datetimestamp,
                                                             Description = description, ID = id, Origin = origin, Heading = heading)
                                           }
                                           l
                                       })
                         if (length(filelist) > 1)
                             tdcl <- new("TextDocCol", .Data = unlist(tdl, recursive = FALSE))
                         else
                             tdcl <- new("TextDocCol", .Data = tdl)
                     },
                     # Read in text documents in XML Reuters Corpus Volume 1 (RCV1) format
                     # The first argument is a directory with the RCV1 XML files
                     "RCV1" = {
                         filelist <- dir(object, pattern = ".xml", full.names = TRUE)
                         tdl <- sapply(filelist,
                                       function(file) {
                                           tree <- xmlTreeParse(file)
                                           xmlApply(xmlRoot(tree), parseNewsItemPlain, stripWhiteSpace, toLower)
                                       })
                         if (length(filelist) > 1)
                             tdcl <- new("TextDocCol", .Data = unlist(tdl, recursive = FALSE))
                         else
                             tdcl <- new("TextDocCol", .Data = tdl)
                     },
                     # Read in text documents in Reuters-21578 XML (not SGML) format
                     # Typically the first argument will be a directory where we can
                     # find the files reut2-000.xml ... reut2-021.xml
                     "REUT21578" = {
                         filelist <- dir(object, pattern = ".xml", full.names = TRUE)
                         tdl <- sapply(filelist,
                                       function(file) {
                                           tree <- xmlTreeParse(file)
                                           xmlApply(xmlRoot(tree), parseReutersPlain, stripWhiteSpace, toLower)
                                       })
                         if (length(filelist) > 1)
                             tdcl <- new("TextDocCol", .Data = unlist(tdl, recursive = FALSE))
                         else
                             tdcl <- new("TextDocCol", .Data = tdl)
                     },
                     "REUT21578_XML" = {
                         filelist <- dir(object, pattern = ".xml", full.names = TRUE)
                         tdl <- sapply(filelist,
                                       function(file) {
                                           parseReutersXML(file)
                                       })
                         tdcl <- new("TextDocCol", .Data = tdl)
                     },
                     # Read in HTML documents as used by http://ris.bka.gv.at/vwgh
                     "RIS" = {
                         filelist <- dir(object, pattern = ".html", full.names = TRUE)
                         tdl <- sapply(filelist,
                                       function(file) {
                                           # Ignore warnings from misformed HTML documents
                                           suppressWarnings(RISDoc <- parseRISPlain(file, stripWhiteSpace, toLower))
                                           if (!is.null(RISDoc)) {
                                               l <- list()
                                               l[[length(l) + 1]] <- RISDoc
                                               l
                                           }
                                       })
                         tdcl <- new("TextDocCol", .Data = tdl)
                     })
              tdcl
          })

# Parse an Austrian RIS HTML document
parseRISPlain <- function(file, stripWhiteSpace = FALSE, toLower = FALSE) {
    author <- ""
    datetimestamp <- date()
    description <- ""

    tree <- htmlTreeParse(file)
    htmlElem <- unlist(tree$children$html$children)

    if (is.null(htmlElem))
        stop(paste("Empty document", file, "cannot be processed."))

    textElem <- htmlElem[which(regexpr("text.value", names(htmlElem)) > 0)]
    names(textElem) <- NULL

    corpus <- paste(textElem, collapse = " ")

    year <- substring(corpus, regexpr("..../../", corpus), regexpr("..../../", corpus) + 3)
    senat <- substring(corpus, regexpr("..../../", corpus) + 5, regexpr("..../../", corpus) + 6)
    number <- substring(corpus, regexpr("..../../", corpus) + 8, regexpr("..../../", corpus) + 11)

    id <- as.integer(paste(year, senat, number, sep = ""))

    if (is.na(id))
        stop(paste("Cannot extract 'Geschaeftszahl' out of malformed document", file))
    origin <- ""

    if (stripWhiteSpace)
        corpus <- gsub("[[:space:]]+", " ", corpus)
    if (toLower)
        corpus <- tolower(corpus)

    heading <- ""

    new("PlainTextDocument", .Data = corpus, Author = author, DateTimeStamp = datetimestamp,
        Description = description, ID = id, Origin = origin, Heading = heading)
}

# Parse a <newsitem></newsitem> element from a well-formed RCV1 XML file
parseNewsItemPlain <- function(node, stripWhiteSpace = FALSE, toLower = FALSE) {
    author <- "Not yet implemented"
    datetimestamp <- xmlAttrs(node)[["date"]]
    description <- "Not yet implemented"
    id <- as.integer(xmlAttrs(node)[["itemid"]])
    origin <- "Reuters Corpus Volume 1 XML"
    corpus <- unlist(xmlApply(node[["text"]], xmlValue), use.names = FALSE)

    if (stripWhiteSpace)
        corpus <- gsub("[[:space:]]+", " ", corpus)
    if (toLower)
        corpus <- tolower(corpus)

    heading <- xmlValue(node[["title"]])

    new("PlainTextDocument", .Data = corpus, Author = author, DateTimeStamp = datetimestamp,
        Description = description, ID = id, Origin = origin, Heading = heading)
}

# Parse a <REUTERS></REUTERS> element from a well-formed Reuters-21578 XML file
parseReutersPlain <- function(node, stripWhiteSpace = FALSE, toLower = FALSE) {
    # The <AUTHOR></AUTHOR> tag is unfortunately NOT obligatory!
    if (!is.null(node[["TEXT"]][["AUTHOR"]]))
        author <- xmlValue(node[["TEXT"]][["AUTHOR"]])
    else
        author <- ""

    datetimestamp <- xmlValue(node[["DATE"]])
    description <- ""
    id <- as.integer(xmlAttrs(node)[["NEWID"]])

    origin <- "Reuters-21578 XML"

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

    topics <- unlist(xmlApply(node[["TOPICS"]], function(x) xmlValue(x)), use.names = FALSE)

    new("PlainTextDocument", .Data = corpus, Cached = 1, Author = author, DateTimeStamp = datetimestamp,
        Description = description, ID = id, Origin = origin, Heading = heading, LocalMetaData = list(Topics = topics))
}

# Parse a <REUTERS></REUTERS> element from a well-formed Reuters-21578 XML file
parseReutersXML<- function(file) {
    new("XMLTextDocument", FileName = file, Cached = 0, Author = "REUTERS", DateTimeStamp = date(),
        Description = "Reuters21578 file containing several news articles", ID = as.integer(0),
        Origin = "Reuters-21578 XML", Heading = "Reuters21578 news articles")
}

setGeneric("loadFileIntoMem", function(object) standardGeneric("loadFileIntoMem"))
setMethod("loadFileIntoMem",
          c("XMLTextDocument"),
          function(object) {
              if (object@Cached == 0) {
                  file <- object@FileName
                  doc <- xmlTreeParse(file)
                  class(doc) <- "list"
                  object@.Data <- doc
                  object@Cached <- 1
                  return(object)
              } else {
                  return(object)
              }
          })

setGeneric("transformTextDocCol", function(object, FUN, ...) standardGeneric("transformTextDocCol"))
setMethod("transformTextDocCol",
          c("TextDocCol"),
          function(object, FUN, ...) {
              lapply(object, FUN, ...)
          })

setGeneric("toPlainTextDocument", function(object, FUN, ...) standardGeneric("toPlainTextDocument"))
setMethod("toPlainTextDocument",
          c("PlainTextDocument"),
          function(object, FUN, ...) {
              return(object)
          })
setMethod("toPlainTextDocument",
          c("XMLTextDocument"),
          function(object, FUN, ...) {
              if (object@Cached == 0)
                  object <- loadFileIntoMem(object)

              corpus <- object@.Data

              # As XMLDocument is no native S4 class, restore valid information
              class(corpus) <- "XMLDocument"
              names(corpus) <- c("doc","dtd")

              return(xmlApply(xmlRoot(corpus), FUN, ...))
          })

setGeneric("stemTextDocument", function(object) standardGeneric("stemTextDocument"))
setMethod("stemTextDocument",
          c("PlainTextDocument"),
          function(object) {
              require(Rstem)
              splittedCorpus <- unlist(strsplit(object, " ", fixed = TRUE))
              stemmedCorpus <- wordStem(splittedCorpus)
              object@.Data <- paste(stemmedCorpus, collapse = " ")
              return (object)
          })

setGeneric("removeStopWordsInTextDocument", function(object, stopwords) standardGeneric("removeStopWordsInTextDocument"))
setMethod("removeStopWordsInTextDocument",
          c("PlainTextDocument", "character"),
          function(object, stopwords) {
              require(Rstem)
              splittedCorpus <- unlist(strsplit(object, " ", fixed = TRUE))
              noStopwordsCorpus <- splittedCorpus[!splittedCorpus %in% stopwords]
              object@.Data <- paste(noStopwordsCorpus, collapse = " ")
              return (object)
          })

setGeneric("filterTextDocCol", function(object, FUN, ...) standardGeneric("filterTextDocCol"))
setMethod("filterTextDocCol",
          c("TextDocCol"),
          function(object, FUN, ...) {
              sapply(object, FUN, ...)
          })

setGeneric("filterREUT21578Topics", function(object, topics, ...) standardGeneric("filterREUT21578Topics"))
setMethod("filterREUT21578Topics",
          c("PlainTextDocument", "character"),
          function(object, topics, ...) {
              if (object@Cached == 0)
                  object <- loadFileIntoMem(object)

              if (any(object@LocalMetaData$Topics %in% topics))
                  return(TRUE)
              else
                  return(FALSE)
          })
