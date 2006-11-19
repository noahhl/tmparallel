# Author: Ingo Feinerer

# The "..." are additional arguments for the function_generator parser
setGeneric("TextDocCol", function(object, parser = plaintext_parser, ...) standardGeneric("TextDocCol"))
setMethod("TextDocCol",
          signature(object = "Source"),
          function(object, parser = plaintext_parser, ...) {
              if (inherits(parser, "function_generator"))
                  parser <- parser(...)

              tdl <- list()
              counter <- 1
              while (!eoi(object)) {
                  object <- step_next(object)
                  elem <- get_elem(object)
                  # If there is no Load on Demand support
                  # we need to load the corpus into memory at startup
                  if (object@LoDSupport)
                      load <- object@Load
                  else
                      load <- TRUE
                  tdl <- c(tdl, list(parser(elem, object@LoDSupport, load, as.character(counter))))
                  counter <- counter + 1
              }

              dmeta.df <- data.frame(MetaID = rep(0, length(tdl)))
              dcmeta.node <- new("MetaDataNode",
                            NodeID = 0,
                            MetaData = list(create_date = date(), creator = Sys.getenv("LOGNAME")),
                            children = list())

              return(new("TextDocCol", .Data = tdl, DMetaData = dmeta.df, DCMetaData = dcmeta.node))
          })

setGeneric("DirSource", function(directory, load = FALSE) standardGeneric("DirSource"))
setMethod("DirSource",
          signature(directory = "character"),
          function(directory, load = FALSE) {
              new("DirSource", LoDSupport = TRUE, FileList = dir(directory, full.names = TRUE),
                  Position = 0, Load = load)
          })

setGeneric("CSVSource", function(object) standardGeneric("CSVSource"))
setMethod("CSVSource",
          signature(object = "character"),
          function(object) {
              object <- substitute(file(object))
              con <- eval(object)
              content <- scan(con, what = "character")
              close(con)
              new("CSVSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })
setMethod("CSVSource",
          signature(object = "ANY"),
          function(object) {
              object <- substitute(object)
              con <- eval(object)
              content <- scan(con, what = "character")
              close(con)
              new("CSVSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })

setGeneric("ReutersSource", function(object) standardGeneric("ReutersSource"))
setMethod("ReutersSource",
          signature(object = "character"),
          function(object) {
              object <- substitute(file(object))
              con <- eval(object)
              corpus <- paste(readLines(con), "\n", collapse = "")
              close(con)
              tree <- xmlTreeParse(corpus, asText = TRUE)
              content <- xmlRoot(tree)$children

              new("ReutersSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })
setMethod("ReutersSource",
          signature(object = "ANY"),
          function(object) {
              object <- substitute(object)
              con <- eval(object)
              corpus <- paste(readLines(con), "\n", collapse = "")
              close(con)
              tree <- xmlTreeParse(corpus, asText = TRUE)
              content <- xmlRoot(tree)$children

              new("ReutersSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })

setGeneric("step_next", function(object) standardGeneric("step_next"))
setMethod("step_next",
          signature(object = "DirSource"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })
setMethod("step_next",
          signature(object = "CSVSource"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })
setMethod("step_next",
          signature(object = "ReutersSource"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })

setGeneric("get_elem", function(object) standardGeneric("get_elem"))
setMethod("get_elem",
          signature(object = "DirSource"),
          function(object) {
              filename <- object@FileList[object@Position]
              list(content = readLines(object@FileList[object@Position]),
                   uri = substitute(file(filename)))
          })
setMethod("get_elem",
          signature(object = "CSVSource"),
          function(object) {
              list(content = object@Content[object@Position],
                   uri = object@URI)
          })
setMethod("get_elem",
          signature(object = "ReutersSource"),
          function(object) {
              # Construct a character representation from the XMLNode
              con <- textConnection("virtual.file", "w")
              saveXML(object@Content[[object@Position]], con)
              close(con)

              list(content = virtual.file, uri = object@URI)
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
          signature(object = "ReutersSource"),
          function(object) {
              if (length(object@Content) <= object@Position)
                  return(TRUE)
              else
                  return(FALSE)
          })

plaintext_parser <- function(...) {
    function(elem, lodsupport, load, id) {
        if (!lodsupport || (lodsupport && load)) {
            doc <- new("PlainTextDocument", .Data = elem$content, URI = elem$uri, Cached = TRUE,
                       Author = "", DateTimeStamp = date(), Description = "", ID = id, Origin = "", Heading = "")
        }
        else {
            doc <- new("PlainTextDocument", URI = elem$uri, Cached = FALSE,
                       Author = "", DateTimeStamp = date(), Description = "", ID = id, Origin = "", Heading = "")
        }

        return(doc)
    }
}
class(plaintext_parser) <- "function_generator"

reut21578xml_parser <- function(...) {
    function(elem, lodsupport, load, id) {
        corpus <- paste(elem$content, "\n", collapse = "")
        tree <- xmlTreeParse(corpus, asText = TRUE)
        node <- xmlRoot(tree)

        # Mask as list to bypass S4 checks
        class(tree) <- "list"

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
            doc <- new("XMLTextDocument", .Data = tree, URI = elem$uri, Cached = TRUE, Author = author,
                       DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters-21578 XML",
                       Heading = heading, LocalMetaData = list(Topics = topics))
        } else {
            doc <- new("XMLTextDocument", URI = elem$uri, Cached = FALSE, Author = author,
                       DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters-21578 XML",
                       Heading = heading, LocalMetaData = list(Topics = topics))
        }

        return(doc)
    }
}
class(reut21578xml_parser) <- "function_generator"

rcv1_parser <- function(...) {
    function(elem, lodsupport, load, id) {
        corpus <- paste(elem$content, "\n", collapse = "")
        tree <- xmlTreeParse(corpus, asText = TRUE)
        node <- xmlRoot(tree)

        # Mask as list to bypass S4 checks
        class(tree) <- "list"

        datetimestamp <- xmlAttrs(node)[["date"]]
        id <- xmlAttrs(node)[["itemid"]]
        heading <- xmlValue(node[["title"]])

        if (!lodsupport || (lodsupport && load)) {
            doc <- new("XMLTextDocument", .Data = tree, URI = elem$uri, Cached = TRUE, Author = "",
                       DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters Corpus Volume 1 XML",
                       Heading = heading)
        } else {
            doc <- new("XMLTextDocument", URI = elem$uri, Cached = FALSE, Author = "",
                       DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters Corpus Volume 1 XML",
                       Heading = heading)
        }

        return(doc)
    }
}
class(rcv1_parser) <- "function_generator"

newsgroup_parser <- function(...) {
    function(elem, lodsupport, load, id) {
        mail <- elem$content
        author <- gsub("From: ", "", grep("^From:", mail, value = TRUE))
        datetimestamp <- gsub("Date: ", "", grep("^Date:", mail, value = TRUE))
        origin <- gsub("Path: ", "", grep("^Path:", mail, value = TRUE))
        heading <- gsub("Subject: ", "", grep("^Subject:", mail, value = TRUE))
        newsgroup <- gsub("Newsgroups: ", "", grep("^Newsgroups:", mail, value = TRUE))

        if (!lodsupport || (lodsupport && load)) {
            # The header is separated from the body by a blank line.
            # Reference: \url{http://en.wikipedia.org/wiki/E-mail#Internet_e-mail_format}
            for (index in seq(along = mail)) {
                if (mail[index] == "")
                    break
            }
            content <- mail[(index + 1):length(mail)]

            doc <- new("NewsgroupDocument", .Data = content, URI = elem$uri, Cached = TRUE,
                       Author = author, DateTimeStamp = datetimestamp,
                       Description = "", ID = id, Origin = origin,
                       Heading = heading, Newsgroup = newsgroup)
        } else {
            doc <- new("NewsgroupDocument", URI = elem$uri, Cached = FALSE, Author = author, DateTimeStamp = datetimestamp,
                       Description = "", ID = id, Origin = origin, Heading = heading, Newsgroup = newsgroup)
        }

        return(doc)
    }
}
class(newsgroup_parser) <- "function_generator"

# Parse a <newsitem></newsitem> element from a well-formed RCV1 XML file
rcv1_to_plain <- function(node, ...) {
    datetimestamp <- xmlAttrs(node)[["date"]]
    id <- xmlAttrs(node)[["itemid"]]
    origin <- "Reuters Corpus Volume 1 XML"
    corpus <- unlist(xmlApply(node[["text"]], xmlValue), use.names = FALSE)
    heading <- xmlValue(node[["title"]])

    new("PlainTextDocument", .Data = corpus, Cached = TRUE, URI = "", Author = "", DateTimeStamp = datetimestamp,
        Description = "", ID = id, Origin = "Reuters Corpus Volume 1 XML", Heading = heading)
}

# Parse a <REUTERS></REUTERS> element from a well-formed Reuters-21578 XML file
reut21578xml_to_plain <- function(node, ...) {
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

    new("PlainTextDocument", .Data = corpus, Cached = TRUE, URI = "", Author = author, DateTimeStamp = datetimestamp,
        Description = description, ID = id, Origin = origin, Heading = heading, LocalMetaData = list(Topics = topics))
}

setGeneric("load_doc", function(object, ...) standardGeneric("load_doc"))
setMethod("load_doc",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              if (!Cached(object)) {
                  con <- eval(URI(object))
                  corpus <- readLines(con)
                  close(con)
                  Corpus(object) <- corpus
                  Cached(object) <- TRUE
                  return(object)
              } else {
                  return(object)
              }
          })
setMethod("load_doc",
          signature(object =  "XMLTextDocument"),
          function(object, ...) {
              if (!Cached(object)) {
                  con <- eval(URI(object))
                  corpus <- paste(readLines(con), "\n", collapse = "")
                  close(con)
                  doc <- xmlTreeParse(corpus, asText = TRUE)
                  class(doc) <- "list"
                  Corpus(object) <- doc
                  Cached(object) <- TRUE
                  return(object)
              } else {
                  return(object)
              }
          })
setMethod("load_doc",
          signature(object = "NewsgroupDocument"),
          function(object, ...) {
              if (!Cached(object)) {
                  con <- eval(URI(object))
                  mail <- readLines(con)
                  close(con)
                  Cached(object) <- TRUE
                  for (index in seq(along = mail)) {
                      if (mail[index] == "")
                          break
                  }
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
              result <- as(lapply(object, FUN, ..., DMetaData = DMetaData(object)), "TextDocCol")
              result@DMetaData <- DMetaData(object)
              return(result)
          })

setGeneric("as.plaintext_doc", function(object, FUN, ...) standardGeneric("as.plaintext_doc"))
setMethod("as.plaintext_doc",
          signature(object = "PlainTextDocument"),
          function(object, FUN, ...) {
              return(object)
          })
setMethod("as.plaintext_doc",
          signature(object = "XMLTextDocument", FUN = "function"),
          function(object, FUN, ...) {
              if (!Cached(object))
                  object <- load_doc(object)

              corpus <- Corpus(object)

              # As XMLDocument is no native S4 class, restore valid information
              class(corpus) <- "XMLDocument"
              names(corpus) <- c("doc","dtd")

              return(FUN(xmlRoot(corpus), ...))
          })

setGeneric("tm_tolower", function(object, ...) standardGeneric("tm_tolower"))
setMethod("tm_tolower",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              if (!Cached(object))
                  object <- load_doc(object)

              Corpus(object) <- tolower(object)
              return(object)
          })

setGeneric("strip_whitespace", function(object, ...) standardGeneric("strip_whitespace"))
setMethod("strip_whitespace",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              if (!Cached(object))
                  object <- load_doc(object)

              Corpus(object) <- gsub("[[:space:]]+", " ", object)
              return(object)
          })

setGeneric("stem_doc", function(object, ...) standardGeneric("stem_doc"))
setMethod("stem_doc",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              if (!Cached(object))
                  object <- load_doc(object)

              require(Rstem)
              splittedCorpus <- unlist(strsplit(object, " ", fixed = TRUE))
              stemmedCorpus <- wordStem(splittedCorpus)
              Corpus(object) <- paste(stemmedCorpus, collapse = " ")
              return(object)
          })

setGeneric("remove_words", function(object, stopwords, ...) standardGeneric("remove_words"))
setMethod("remove_words",
          signature(object = "PlainTextDocument", stopwords = "character"),
          function(object, stopwords, ...) {
              if (!Cached(object))
                  object <- load_doc(object)

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
              indices <- sapply(object, FUN, ..., DMetaData = DMetaData(object))
              object[indices]
          })

setGeneric("tm_index", function(object, ..., FUN = s_filter) standardGeneric("tm_index"))
setMethod("tm_index",
          signature(object = "TextDocCol"),
          function(object, ..., FUN = s_filter) {
              sapply(object, FUN, ..., DMetaData = DMetaData(object))
          })

s_filter <- function(object, s, ..., DMetaData) {
    b <- TRUE
    for (tag in names(s)) {
        if (tag %in% names(LocalMetaData(object))) {
            b <- b && any(grep(s[[tag]], LocalMetaData(object)[[tag]]))
        } else if (tag %in% names(DMetaData)){
            b <- b && any(grep(s[[tag]], DMetaData[[tag]]))
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
                  object <- load_doc(object)

              return(any(grep(pattern, Corpus(object))))
          })

setGeneric("attach_data", function(object, data) standardGeneric("attach_data"))
setGeneric("attach_metadata", function(object, name, metadata) standardGeneric("attach_metadata"))

setGeneric("append_doc", function(object, data, meta) standardGeneric("append_doc"))
setMethod("append_doc",
          signature(object = "TextDocCol", data = "TextDocument", meta = "list"),
          function(object, data, meta) {
              object@.Data <- c(object@.Data, list(data))
              object@DMetaData <- rbind(object@DMetaData, c(MetaID = DCMetaData(object)@NodeID, meta))
              return(object)
          })

setGeneric("append_meta", function(object, dcmeta, dmeta) standardGeneric("append_meta"))
setMethod("append_meta",
          signature(object = "TextDocCol", dcmeta = "list", dmeta = "list"),
          function(object, dcmeta, dmeta) {
              object@DCMetaData@MetaData <- c(object@DCMetaData@MetaData, dcmeta)
              object@DMetaData <- cbind(object@DMetaData, dmeta)
              return(object)
          })

setGeneric("remove_metadata", function(object, name) standardGeneric("remove_metadata"))
#setMethod("remove_metadata",
#          signature(object = "TextDocCol"),
#          function(object, name) {
#              object@DMetaData <- DMetaData(object)[names(DMetaData(object)) != name]
#              return(object)
#          })

setGeneric("modify_metadata", function(object, name, metadata) standardGeneric("modify_metadata"))
#setMethod("modify_metadata",
#          signature(object = "TextDocCol"),
#          function(object, name, metadata) {
#              object@DMetaData[[name]] <- metadata
#              return(object)
#          })

setMethod("[",
          signature(x = "TextDocCol", i = "ANY", j = "ANY", drop = "ANY"),
          function(x, i, j, ... , drop) {
              if(missing(i))
                  return(x)

              object <- x
              object@.Data <- x@.Data[i, ..., drop = FALSE]
              object@DMetaData <- DMetaData(object)[i, ]
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

# Update \code{NodeID}s of a DCMetaData tree
# TODO: Avoid global variables outside of update_id function
update_id <- function(object) {
    id <<- 0
    mapping <<- left.mapping <<- NULL
    level <<- 0
    return(list(root = set_id(object), left.mapping = left.mapping, right.mapping = mapping))
}

# Traversal of (binary) DCMetaData tree with setup of \code{NodeID}s
set_id <- function(object) {
    object@NodeID <- id
    id <<- id + 1
    level <<- level + 1

    if (length(object@children) > 0) {
        mapping <<- cbind(mapping, c(object@children[[1]]@NodeID, id))
        left <- set_id(object@children[[1]])
        if (level == 1) {
            left.mapping <<- mapping
            mapping <<- NULL
        }
        mapping <<- cbind(mapping, c(object@children[[2]]@NodeID, id))
        right <- set_id(object@children[[2]])

        object@children <- list(left, right)
    }
    level <<- level - 1

    return(object)
}

setMethod("c",
          signature(x = "TextDocCol"),
          function(x, y, ..., meta = list(merge_date = date(), merger = Sys.getenv("LOGNAME")), recursive = TRUE) {
              if (!inherits(y, "TextDocCol"))
                  stop("invalid argument")

              object <- x
              # Concatenate data slots
              object@.Data <- c(as(x, "list"), as(y, "list"))

              # Update the DCMetaData tree
              dcmeta <- new("MetaDataNode", NodeID = 0, MetaData = meta, children = list(DCMetaData(x), DCMetaData(y)))
              update.struct <- update_id(dcmeta)
              object@DCMetaData <- update.struct$root

              # Find indices to be updated for the left tree
              indices.mapping <- NULL
              for (m in levels(as.factor(DMetaData(x)$MetaID))) {
                  indices <- (DMetaData(x)$MetaID == m)
                  indices.mapping <- c(indices.mapping, list(m = indices))
                  names(indices.mapping)[length(indices.mapping)] <- m
              }

              # Update the DMetaData data frames for the left tree
              for (i in 1:ncol(update.struct$left.mapping)) {
                  map <- update.struct$left.mapping[,i]
                  x@DMetaData$MetaID <- replace(DMetaData(x)$MetaID, indices.mapping[[as.character(map[1])]], map[2])
              }

              # Find indices to be updated for the right tree
              indices.mapping <- NULL
              for (m in levels(as.factor(DMetaData(y)$MetaID))) {
                  indices <- (DMetaData(y)$MetaID == m)
                  indices.mapping <- c(indices.mapping, list(m = indices))
                  names(indices.mapping)[length(indices.mapping)] <- m
              }

              # Update the DMetaData data frames for the right tree
              for (i in 1:ncol(update.struct$right.mapping)) {
                  map <- update.struct$right.mapping[,i]
                  y@DMetaData$MetaID <- replace(DMetaData(y)$MetaID, indices.mapping[[as.character(map[1])]], map[2])
              }

              # Merge the DMetaData data frames
              labels <- setdiff(names(DMetaData(y)), names(DMetaData(x)))
              na.matrix <- matrix(NA, nrow = nrow(DMetaData(x)), ncol = length(labels), dimnames = list(row.names(DMetaData(x)), labels))
              x.dmeta.aug <- cbind(DMetaData(x), na.matrix)
              labels <- setdiff(names(DMetaData(x)), names(DMetaData(y)))
              na.matrix <- matrix(NA, nrow = nrow(DMetaData(y)), ncol = length(labels), dimnames = list(row.names(DMetaData(y)), labels))
              y.dmeta.aug <- cbind(DMetaData(y), na.matrix)
              object@DMetaData <- rbind(x.dmeta.aug, y.dmeta.aug)

              return(object)
    })
#setMethod("c",
#          signature(x = "TextDocument"),
#          function(x, ..., recursive = TRUE){
#              args <- list(...)
#              if(length(args) == 0)
#                  return(x)
#              return(new("TextDocCol", .Data = list(x, ...)))
#    })

setMethod("length",
          signature(x = "TextDocCol"),
          function(x){
              return(length(as(x, "list")))
    })

setMethod("show",
          signature(object = "TextDocCol"),
          function(object){
              cat(sprintf(ngettext(length(object),
                                   "A text document collection with %d text document\n",
                                   "A text document collection with %d text documents\n"),
                          length(object)))
    })

setMethod("summary",
          signature(object = "TextDocCol"),
          function(object){
              show(object)
              if (length(DMetaData(object)) > 0) {
                  cat(sprintf(ngettext(length(DMetaData(object)),
                                              "\nThe global metadata consists of %d tag-value pair\n",
                                              "\nThe global metadata consists of %d tag-value pairs\n"),
                                       length(DMetaData(object))))
                  cat("Available tags are:\n")
                  cat(names(DMetaData(object)), "\n")
              }
    })

setGeneric("inspect", function(object) standardGeneric("inspect"))
setMethod("inspect",
          signature("TextDocCol"),
          function(object) {
              summary(object)
              cat("\n")
              show(as(object, "list"))
          })

# No metadata is checked
setGeneric("%IN%", function(x, y) standardGeneric("%IN%"))
setMethod("%IN%",
          signature(x = "TextDocument", y = "TextDocCol"),
          function(x, y) {
              x %in% y
          })
