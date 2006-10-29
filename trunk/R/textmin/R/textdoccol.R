# Author: Ingo Feinerer

# ... fuer Argumente des Funktionengenerators
setGeneric("TextDocCol", function(object, parser = plaintext_parser, ...) standardGeneric("TextDocCol"))
setMethod("TextDocCol",
          signature(object = "Source"),
          function(object, parser = plaintext_parser) {
              if (inherits(parser, "function_generator"))
                  parser <- parser(...)

              tdl <- list()
              counter <- 1
              while (!eoi(object)) {
                  object <- stepNext(object)
                  elem <- getElem(object)
                  # If there is no Load on Demand support
                  # we need to load the corpus into memory at startup
                  if (object@LoDSupport)
                      load <- object@Load
                  else
                      load <- TRUE
                  tdl <- c(tdl, list(parser(elem, object@LoDSupport, load, as.character(counter))))
                  counter <- counter + 1
              }

              return(new("TextDocCol", .Data = tdl))
          })

setGeneric("DirSource", function(directory, load = FALSE) standardGeneric("DirSource"))
setMethod("DirSource",
          signature(directory = "character"),
          function(directory, load = FALSE) {
              new("DirSource", LoDSupport = TRUE, FileList = dir(directory, full.names = TRUE),
                  Position = 0, Load = load)
          })

setGeneric("CSVSource", function(file) standardGeneric("CSVSource"))
setMethod("CSVSource",
          signature(file = "character"),
          function(file) {
              new("CSVSource", LoDSupport = FALSE, FileName = file,
                  Content = scan(file, what = "character"), Position = 0)
          })

setGeneric("Reuters21578XMLSource", function(file) standardGeneric("Reuters21578XMLSource"))
setMethod("Reuters21578XMLSource",
          signature(file = "character"),
          function(file) {
              tree <- xmlTreeParse(file)
              content <- xmlRoot(tree)$children
              new("Reuters21578XMLSource", LoDSupport = FALSE, FileName = file,
                  Content = content, Position = 0)
          })

setGeneric("stepNext", function(object) standardGeneric("stepNext"))
setMethod("stepNext",
          signature(object = "DirSource"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })
setMethod("stepNext",
          signature(object = "CSVSource"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })
setMethod("stepNext",
          signature(object = "Reuters21578XMLSource"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })

setGeneric("getElem", function(object) standardGeneric("getElem"))
setMethod("getElem",
          signature(object = "DirSource"),
          function(object) {
              list(content = readLines(object@FileList[object@Position]),
                   filename = object@FileList[object@Position])
          })
setMethod("getElem",
          signature(object = "CSVSource"),
          function(object) {
              list(content = object@Content[object@Position],
                   filename = object@FileName)
          })
setMethod("getElem",
          signature(object = "Reuters21578XMLSource"),
          function(object) {
              list(content = object@Content[object@Position],
                   filename = object@FileName)
          })

setGeneric("eoi", function(object) standardGeneric("eoi"))
setMethod("eoi",
          signature(object = "DirSource"),
          function(object) {
              if (length(object@FileList) <= object@Position)
                  return(TRUE)
              else
                  return(FALSE)
          })
setMethod("eoi",
          signature(object = "CSVSource"),
          function(object) {
              if (length(object@Content) <= object@Position)
                  return(TRUE)
              else
                  return(FALSE)
          })
setMethod("eoi",
          signature(object = "Reuters21578XMLSource"),
          function(object) {
              if (length(object@Content) <= object@Position)
                  return(TRUE)
              else
                  return(FALSE)
          })

plaintext_parser <- function(...) {
    function(elem, lodsupport, load, id) {
        if (!lodsupport || (lodsupport && load)) {
            doc <- new("PlainTextDocument", .Data = elem$content, FileName = elem$filename, Cached = TRUE,
                       Author = "", DateTimeStamp = date(), Description = "", ID = id, Origin = "", Heading = "")
        }
        else {
            doc <- new("PlainTextDocument", FileName = elem$filename, Cached = FALSE,
                       Author = "", DateTimeStamp = date(), Description = "", ID = id, Origin = "", Heading = "")
        }

        return(doc)
    }
}
class(plaintext_parser) <- "function_generator"

reuters21578xml_parser <- function(...) {
    function(elem, lodsupport, load, id) {
        tree <- xmlTreeParse(elem$filename)
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

        if (!lodsupport || (lodsupport && load)) {
            doc <- new("XMLTextDocument", .Data = elem$content, FileName = elem$filename, Cached = TRUE, Author = author,
                       DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters-21578 XML",
                       Heading = heading, LocalMetaData = list(Topics = topics))
        } else {
            doc <- new("XMLTextDocument", FileName = elem$filename, Cached = FALSE, Author = author,
                       DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters-21578 XML",
                       Heading = heading, LocalMetaData = list(Topics = topics))
        }

        return(doc)
    }
}
class(reuters21578xml_parser) <- "function_generator"

rcv1_parser <- function(...) {
    function(elem, lodsupport, load, id) {
        tree <- xmlTreeParse(elem$filename)
        node <- xmlRoot(tree)

        datetimestamp <- xmlAttrs(node)[["date"]]
        id <- xmlAttrs(node)[["itemid"]]
        heading <- xmlValue(node[["title"]])

        if (!lodsupport || (lodsupport && load)) {
            doc <- new("XMLTextDocument", .Data = elem$content, FileName = elem$filename, Cached = TRUE, Author = "",
                       DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters Corpus Volume 1 XML",
                       Heading = heading)
        } else {
            doc <- new("XMLTextDocument", FileName = elem$filename, Cached = FALSE, Author = "",
                       DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters Corpus Volume 1 XML",
                       Heading = heading)
        }

        return(doc)
    }
}
class(rcv1_parser) <- "function_generator"

uci_kdd_newsgroup_parser <- function(...) {
    function(elem, lodsupport, load, id) {
        mail <- readLines(elem$filename)
        author <- gsub("From: ", "", grep("^From:", mail, value = TRUE))
        datetimestamp <- gsub("Date: ", "", grep("^Date:", mail, value = TRUE))
        origin <- gsub("Path: ", "", grep("^Path:", mail, value = TRUE))
        heading <- gsub("Subject: ", "", grep("^Subject:", mail, value = TRUE))
        newsgroup <- gsub("Newsgroups: ", "", grep("^Newsgroups:", mail, value = TRUE))

        if (!lodsupport || (lodsupport && load)) {
            index <- grep("^Lines:", mail)
            content <- mail[(index + 1):length(mail)]

            doc <- new("NewsgroupDocument", .Data = content, FileName = elem$filename, Cached = TRUE,
                       Author = author, DateTimeStamp = datetimestamp,
                       Description = "", ID = elem$filename, Origin = origin,
                       Heading = heading, Newsgroup = newsgroup)
        } else {
            doc <- new("NewsgroupDocument", FileName = elem$filename, Cached = FALSE, Author = author, DateTimeStamp = datetimestamp,
                       Description = "", ID = elem$filename, Origin = origin, Heading = heading, Newsgroup = newsgroup)
        }

        return(doc)
    }
}
class(uci_kdd_newsgroup_parser) <- "function_generator"

# Parse a <newsitem></newsitem> element from a well-formed RCV1 XML file
rcv1_to_plain <- function(node) {
    datetimestamp <- xmlAttrs(node)[["date"]]
    id <- xmlAttrs(node)[["itemid"]]
    origin <- "Reuters Corpus Volume 1 XML"
    corpus <- unlist(xmlApply(node[["text"]], xmlValue), use.names = FALSE)
    heading <- xmlValue(node[["title"]])

    new("PlainTextDocument", .Data = corpus, Author = "", DateTimeStamp = datetimestamp,
        Description = "", ID = id, Origin = "Reuters Corpus Volume 1 XML", Heading = heading)
}

# Parse a <REUTERS></REUTERS> element from a well-formed Reuters-21578 XML file
reuters21578xml_to_plain <- function(node) {
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

setGeneric("loadFileIntoMem", function(object) standardGeneric("loadFileIntoMem"))
setMethod("loadFileIntoMem",
          signature(object = "PlainTextDocument"),
          function(object) {
              if (!Cached(object)) {
                  corpus <- readLines(FileName(object))
                  Corpus(object) <- corpus
                  Cached(object) <- TRUE
                  return(object)
              } else {
                  return(object)
              }
          })
setMethod("loadFileIntoMem",
          signature(object =  "XMLTextDocument"),
          function(object) {
              if (!Cached(object)) {
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
          signature(object = "NewsgroupDocument"),
          function(object) {
              if (!Cached(object)) {
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
          signature(object = "TextDocCol", FUN = "function"),
          function(object, FUN, ...) {
              result <- as(lapply(object, FUN, ..., GlobalMetaData = GlobalMetaData(object)), "TextDocCol")
              result@GlobalMetaData <- GlobalMetaData(object)
              return(result)
          })

setGeneric("toPlainTextDocument", function(object, FUN, ...) standardGeneric("toPlainTextDocument"))
setMethod("toPlainTextDocument",
          signature(object = "PlainTextDocument"),
          function(object, FUN, ...) {
              return(object)
          })
setMethod("toPlainTextDocument",
          signature(object = "XMLTextDocument", FUN = "function"),
          function(object, FUN, ...) {
              if (!Cached(object))
                  object <- loadFileIntoMem(object)

              corpus <- Corpus(object)

              # As XMLDocument is no native S4 class, restore valid information
              class(corpus) <- "XMLDocument"
              names(corpus) <- c("doc","dtd")

              return(FUN(xmlRoot(corpus), ...))
          })

setGeneric("stemTextDocument", function(object, ...) standardGeneric("stemTextDocument"))
setMethod("stemTextDocument",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              if (!Cached(object))
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
          function(object, stopwords, ...) {
              if (!Cached(object))
                  object <- loadFileIntoMem(object)

              require(Rstem)
              splittedCorpus <- unlist(strsplit(object, " ", fixed = TRUE))
              noStopwordsCorpus <- splittedCorpus[!splittedCorpus %in% stopwords]
              Corpus(object) <- paste(noStopwordsCorpus, collapse = " ")
              return(object)
          })

setGeneric("tm_filter", function(object, ..., FUN = s_filter) standardGeneric("tm_filter"))
setMethod("tm_filter",
          signature(object = "TextDocCol"),
          function(object, ..., FUN = s_filter) {
              object[tm_index(object, ..., FUN)]
          })

setGeneric("tm_index", function(object, ..., FUN = s_filter) standardGeneric("tm_index"))
setMethod("tm_index",
          signature(object = "TextDocCol"),
          function(object, ..., FUN = s_filter) {
              sapply(object, FUN, ..., GlobalMetaData = GlobalMetaData(object))
          })

s_filter <- function(object, s, ..., GlobalMetaData) {
    b <- TRUE
    for (tag in names(s)) {
        if (tag %in% names(LocalMetaData(object))) {
            b <- b && any(grep(s[[tag]], LocalMetaData(object)[[tag]]))
        } else if (tag %in% names(GlobalMetaData)){
            b <- b && any(grep(s[[tag]], GlobalMetaData[[tag]]))
        } else {
            b <- b && any(grep(s[[tag]], eval(call(tag, object))))
        }
    }
    return(b)
}

setGeneric("fulltext_search_filter", function(object, pattern, ...) standardGeneric("fulltext_search_filter"))
setMethod("fulltext_search_filter",
          signature(object = "PlainTextDocument", pattern = "character"),
          function(object, pattern, ...) {
              if (!Cached(object))
                  object <- loadFileIntoMem(object)

              return(any(grep(pattern, Corpus(object))))
          })

setGeneric("attachData", function(object, data) standardGeneric("attachData"))
setMethod("attachData",
          signature(object = "TextDocCol", data = "TextDocument"),
          function(object, data) {
              data <- as(list(data), "TextDocCol")
              object@.Data <- as(c(object@.Data, data), "TextDocCol")
              return(object)
          })

setGeneric("attachMetaData", function(object, name, metadata) standardGeneric("attachMetaData"))
setMethod("attachMetaData",
          signature(object = "TextDocCol"),
          function(object, name, metadata) {
              object@GlobalMetaData <- c(GlobalMetaData(object), new = list(metadata))
              names(object@GlobalMetaData)[length(names(GlobalMetaData(object)))] <- name
              return(object)
          })

setGeneric("setSubscriptable", function(object, name) standardGeneric("setSubscriptable"))
setMethod("setSubscriptable",
          signature(object = "TextDocCol"),
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

setMethod("[<-",
          signature(x = "TextDocCol", i = "ANY", j = "ANY", value = "ANY"),
          function(x, i, j, ... , value) {
              object <- x
              object@.Data[i, ...] <- value
              return(object)
          })

setMethod("[[",
          signature(x = "TextDocCol", i = "ANY", j = "ANY"),
          function(x, i, j, ...) {
              return(x@.Data[[i, ...]])
          })

setMethod("[[<-",
          signature(x = "TextDocCol", i = "ANY", j = "ANY", value = "ANY"),
          function(x, i, j, ..., value) {
              object <- x
              object@.Data[[i, ...]] <- value
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
