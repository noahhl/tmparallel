# Author: Ingo Feinerer

setGeneric("TextDocCol", function(object, parser = plaintext.parser, lod = FALSE) standardGeneric("TextDocCol"))
setMethod("TextDocCol",
          signature(object = "character"),
          function(object, parser = plaintext.parser, lod = FALSE) {
              filelist <- dir(object, full.names = TRUE)
              tdl <- lapply(filelist, parser, lod)
              return(new("TextDocCol", .Data = tdl))
          })

plaintext.parser <- function(file, lod) {
    id <- file
    origin <- dirname(file)

    doc <- new("PlainTextDocument", FileName = file, Cached = FALSE, Author = "Unknown",
               DateTimeStamp = date(), Description = "", ID = id, Origin = origin, Heading = "")

    if (lod) {
        doc <- loadFileIntoMem(doc)
    }

    return(doc)
}

reuter21578xml.parser <- function(file, lod) {
    tree <- xmlTreeParse(file)
    node <- xmlRoot(tree)

    # The <AUTHOR></AUTHOR> tag is unfortunately NOT obligatory!
    if (!is.null(node[["TEXT"]][["AUTHOR"]]))
        author <- xmlValue(node[["TEXT"]][["AUTHOR"]])
    else
        author <- ""

    datetimestamp <- xmlValue(node[["DATE"]])
    description <- ""
    id <- xmlAttrs(node)[["NEWID"]]

    # The <TITLE></TITLE> tag is unfortunately NOT obligatory!
    if (!is.null(node[["TEXT"]][["TITLE"]]))
        heading <- xmlValue(node[["TEXT"]][["TITLE"]])
    else
        heading <- ""

    topics <- unlist(xmlApply(node[["TOPICS"]], function(x) xmlValue(x)), use.names = FALSE)

    doc <- new("XMLTextDocument", FileName = file, Cached = FALSE, Author = author,
               DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters-21578 XML",
               Heading = heading, LocalMetaData = list(Topics = topics))

    if (lod) {
        doc <- loadFileIntoMem(doc)
    }

    return(doc)
}

rcv1.parser <- function(file, lod) {
    tree <- xmlTreeParse(file)
    node <- xmlRoot(tree)

    datetimestamp <- xmlAttrs(node)[["date"]]
    id <- xmlAttrs(node)[["itemid"]]
    heading <- xmlValue(node[["title"]])

    doc <- new("XMLTextDocument", FileName = file, Cached = FALSE, Author = "",
               DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters Corpus Volume 1 XML",
               Heading = heading)

    if (lod) {
        doc <- loadFileIntoMem(doc)
    }

    return(doc)
}

uci.kdd.newsgroup.parser <-  function(file, lod) {
    mail <- readLines(file)
    author <- gsub("From: ", "", grep("^From:", mail, value = TRUE))
    datetimestamp <- gsub("Date: ", "", grep("^Date:", mail, value = TRUE))
    origin <- gsub("Path: ", "", grep("^Path:", mail, value = TRUE))
    heading <- gsub("Subject: ", "", grep("^Subject:", mail, value = TRUE))
    newsgroup <- gsub("Newsgroups: ", "", grep("^Newsgroups:", mail, value = TRUE))

    new("NewsgroupDocument", FileName = file, Cached = FALSE, Author = author, DateTimeStamp = datetimestamp,
        Description = "", ID = file, Origin = origin, Heading = heading, Newsgroup = newsgroup)

    if (lod) {
        doc <- loadFileIntoMem(doc)
    }

    return(doc)
}

# Parse a <newsitem></newsitem> element from a well-formed RCV1 XML file
# TODO: Check if it works with example
rcv1.to.plain <- function(node) {
    datetimestamp <- xmlAttrs(node)[["date"]]
    id <- xmlAttrs(node)[["itemid"]]
    origin <- "Reuters Corpus Volume 1 XML"
    corpus <- unlist(xmlApply(node[["text"]], xmlValue), use.names = FALSE)
    heading <- xmlValue(node[["title"]])

    new("PlainTextDocument", .Data = corpus, Author = "", DateTimeStamp = datetimestamp,
        Description = "", ID = id, Origin = "Reuters Corpus Volume 1 XML", Heading = heading)
}

# Parse a <REUTERS></REUTERS> element from a well-formed Reuters-21578 XML file
# TODO: Ensure it works
reuters21578xml.to.plain <- function(node) {
    # The <AUTHOR></AUTHOR> tag is unfortunately NOT obligatory!
    if (!is.null(node[["TEXT"]][["AUTHOR"]]))
        author <- xmlValue(node[["TEXT"]][["AUTHOR"]])
    else
        author <- ""

    datetimestamp <- xmlValue(node[["DATE"]])
    description <- ""
    id <- xmlAttrs(node)[["NEWID"]]

    origin <- "Reuters-21578 XML"

    # The <BODY></BODY> tag is unfortunately NOT obligatory!
    if (!is.null(node[["TEXT"]][["BODY"]]))
        corpus <- xmlValue(node[["TEXT"]][["BODY"]])
    else
        corpus <- ""

    # The <TITLE></TITLE> tag is unfortunately NOT obligatory!
    if (!is.null(node[["TEXT"]][["TITLE"]]))
        heading <- xmlValue(node[["TEXT"]][["TITLE"]])
    else
        heading <- ""

    topics <- unlist(xmlApply(node[["TOPICS"]], function(x) xmlValue(x)), use.names = FALSE)

    new("PlainTextDocument", .Data = corpus, Cached = TRUE, Author = author, DateTimeStamp = datetimestamp,
        Description = description, ID = id, Origin = origin, Heading = heading, LocalMetaData = list(Topics = topics))
}

setGeneric("loadFileIntoMem", function(object, ...) standardGeneric("loadFileIntoMem"))
setMethod("loadFileIntoMem",
          c("PlainTextDocument"),
          function(object, ...) {
              if (Cached(object) == FALSE) {
                  corpus <- readLines(FileName(object))
                  Corpus(object) <- corpus
                  Cached(object) <- TRUE
                  return(object)
              } else {
                  return(object)
              }
          })
setMethod("loadFileIntoMem",
          c("XMLTextDocument"),
          function(object, ...) {
              if (Cached(object) == FALSE) {
                  file <- FileName(object)
                  doc <- xmlTreeParse(file)
                  class(doc) <- "list"
                  Corpus(object) <- doc
                  Cached(object) <- TRUE
                  return(object)
              } else {
                  return(object)
              }
          })
setMethod("loadFileIntoMem",
          c("NewsgroupDocument"),
          function(object, ...) {
              if (Cached(object) == FALSE) {
                  mail <- readLines(FileName(object))
                  Cached(object) <- TRUE
                  index <- grep("^Lines:", mail)
                  Corpus(object) <- mail[(index + 1):length(mail)]
                  return(object)
              } else {
                  return(object)
              }
          })

setGeneric("tm_transform", function(object, FUN, ...) standardGeneric("tm_transform"))
setMethod("tm_transform",
          c("TextDocCol"),
          function(object, FUN, ...) {
              result <- as(lapply(object, FUN, ..., GlobalMetaData = GlobalMetaData(object)), "TextDocCol")
              result@GlobalMetaData <- GlobalMetaData(object)
              return(result)
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
              if (Cached(object) == FALSE)
                  object <- loadFileIntoMem(object)

              corpus <- Corpus(object)

              # As XMLDocument is no native S4 class, restore valid information
              class(corpus) <- "XMLDocument"
              names(corpus) <- c("doc","dtd")

              return(FUN(xmlRoot(corpus), ...)))
          })

setGeneric("stemTextDocument", function(object, ...) standardGeneric("stemTextDocument"))
setMethod("stemTextDocument",
          c("PlainTextDocument"),
          function(object) {
              if (Cached(object) == FALSE)
                  object <- loadFileIntoMem(object)

              require(Rstem)
              splittedCorpus <- unlist(strsplit(object, " ", fixed = TRUE))
              stemmedCorpus <- wordStem(splittedCorpus, ...)
              Corpus(object) <- paste(stemmedCorpus, collapse = " ")
              return(object)
          })

setGeneric("removeStopWords", function(object, stopwords, ...) standardGeneric("removeStopWords"))
setMethod("removeStopWords",
          signature(object = "PlainTextDocument", stopwords = "character"),
          function(object, stopwords) {
              if (Cached(object) == FALSE)
                  object <- loadFileIntoMem(object)

              require(Rstem)
              splittedCorpus <- unlist(strsplit(object, " ", fixed = TRUE))
              noStopwordsCorpus <- splittedCorpus[!splittedCorpus %in% stopwords]
              Corpus(object) <- paste(noStopwordsCorpus, collapse = " ")
              return(object)
          })

setGeneric("tm_filter", function(object, FUN, ...) standardGeneric("tm_filter"))
setMethod("tm_filter",
          c("TextDocCol"),
          function(object, FUN, ...) {
              sapply(object, FUN, ..., GlobalMetaData = GlobalMetaData(object))
          })

setGeneric("filterREUT21578Topics", function(object, topics, ...) standardGeneric("filterREUT21578Topics"))
setMethod("filterREUT21578Topics",
          c("PlainTextDocument", "character"),
          function(object, topics) {
              if (Cached(object) == FALSE)
                  object <- loadFileIntoMem(object)

              if (any(LocalMetaData(object)$Topics %in% topics))
                  return(TRUE)
              else
                  return(FALSE)
          })

setGeneric("filterIDs", function(object, IDs, ...) standardGeneric("filterIDs"))
setMethod("filterIDs",
          c("TextDocument", "numeric"),
          function(object, IDs) {
              if (ID(object) %in% IDs)
                  return(TRUE)
              else
                  return(FALSE)
          })

setGeneric("attachData", function(object, data) standardGeneric("attachData"))
setMethod("attachData",
          c("TextDocCol","TextDocument"),
          function(object, data) {
              data <- as(list(data), "TextDocCol")
              object@.Data <- as(c(object@.Data, data), "TextDocCol")
              return(object)
          })

setGeneric("attachMetaData", function(object, name, metadata) standardGeneric("attachMetaData"))
setMethod("attachMetaData",
          c("TextDocCol"),
          function(object, name, metadata) {
              object@GlobalMetaData <- c(GlobalMetaData(object), new = list(metadata))
              names(object@GlobalMetaData)[length(names(GlobalMetaData(object)))] <- name
              return(object)
          })

setGeneric("setSubscriptable", function(object, name) standardGeneric("setSubscriptable"))
setMethod("setSubscriptable",
          c("TextDocCol"),
          function(object, name) {
              if (!is.character(GlobalMetaData(object)$subscriptable))
                  object <- attachMetaData(object, "subscriptable", name)
              else
                  object@GlobalMetaData$subscriptable <- c(GlobalMetaData(object)$subscriptable, name)
              return(object)
          })

setMethod("[",
          signature(x = "TextDocCol", i = "ANY", j = "ANY", drop = "ANY"),
          function(x, i, j, ... , drop) {
              if(missing(i))
                  return(x)

              object <- x
              object@.Data <- x@.Data[i, ..., drop = FALSE]
              for (m in names(GlobalMetaData(object))) {
                  if (m %in% GlobalMetaData(object)$subscriptable) {
                      object@GlobalMetaData[[m]] <- GlobalMetaData(object)[[m]][i, ..., drop = FALSE]
                  }
              }
              return(object)
          })

setMethod("c",
          signature(x = "TextDocCol"),
          function(x, ..., recursive = TRUE){
              args <- list(...)
              if(length(args) == 0)
                  return(x)
              return(as(c(as(x, "list"), ...), "TextDocCol"))
    })

setMethod("length",
          signature(x = "TextDocCol"),
          function(x){
              return(length(as(x, "list")))
    })

setMethod("show",
          signature(object = "TextDocCol"),
          function(object){
              cat("A text document collection with", length(object), "text document")
              if (length(object) == 1)
                  cat("\n")
              else
                  cat("s\n")
    })

setMethod("summary",
          signature(object = "TextDocCol"),
          function(object){
              show(object)
              if (length(GlobalMetaData(object)) > 0) {
                  cat("\nThe global metadata consists of", length(GlobalMetaData(object)), "tag-value pair")
                  if (length(GlobalMetaData(object)) == 1)
                      cat(".\n")
                  else
                      cat("s.\n")
                  cat("Available tags are:\n")
                  cat(names(GlobalMetaData(object)), "\n")
              }
    })

setGeneric("inspect", function(object) standardGeneric("inspect"))
setMethod("inspect",
          c("TextDocCol"),
          function(object) {
              summary(object)
              cat("\n")
              show(as(object, "list"))
          })
