# Author: Ingo Feinerer

# The "..." are additional arguments for the function_generator parser
setGeneric("TextDocCol", function(object, parser = read_plain, load = FALSE, ...) standardGeneric("TextDocCol"))
setMethod("TextDocCol",
          signature(object = "Source"),
          function(object, parser = read_plain, load = FALSE, ...) {
              if (inherits(parser, "function_generator"))
                  parser <- parser(...)

              tdl <- list()
              counter <- 1
              while (!eoi(object)) {
                  object <- step_next(object)
                  elem <- get_elem(object)
                  # If there is no Load on Demand support
                  # we need to load the corpus into memory at startup
                  if (!object@LoDSupport)
                      load <- TRUE
                  tdl <- c(tdl, list(parser(elem, load, as.character(counter))))
                  counter <- counter + 1
              }

              dmeta.df <- data.frame(MetaID = rep(0, length(tdl)), stringsAsFactors = FALSE)
              dcmeta.node <- new("MetaDataNode",
                            NodeID = 0,
                            MetaData = list(create_date = Sys.time(), creator = Sys.getenv("LOGNAME")),
                            children = list())

              return(new("TextDocCol", .Data = tdl, DMetaData = dmeta.df, DCMetaData = dcmeta.node))
          })

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

setGeneric("tm_update", function(object, origin, parser = read_plain, ...) standardGeneric("tm_update"))
# Update is only supported for directories
# At the moment no other LoD devices are available anyway
setMethod("tm_update",
          signature(object = "TextDocCol", origin = "DirSource"),
          function(object, origin, parser = read_plain, ...) {
              if (inherits(parser, "function_generator"))
                  parser <- parser(...)

              object.filelist <- unlist(lapply(object, function(x) {as.character(URI(x))[2]}))
              new.files <- setdiff(origin@FileList, object.filelist)

              for (filename in new.files) {
                  elem <- list(content = readLines(filename),
                               uri = substitute(file(filename)))
                  object <- append_doc(object, parser(elem, TRUE, origin@Load, filename), NA)
              }

              return(object)
          })

setGeneric("tm_map", function(object, FUN, ...) standardGeneric("tm_map"))
setMethod("tm_map",
          signature(object = "TextDocCol", FUN = "function"),
          function(object, FUN, ...) {
              result <- object
              # Note that text corpora are automatically loaded into memory via \code{[[}
              result@.Data <- lapply(object, FUN, ..., DMetaData = DMetaData(object))
              return(result)
          })

setGeneric("as.plain", function(object, FUN, ...) standardGeneric("as.plain"))
setMethod("as.plain",
          signature(object = "PlainTextDocument"),
          function(object, FUN, ...) {
              return(object)
          })
setMethod("as.plain",
          signature(object = "XMLTextDocument", FUN = "function"),
          function(object, FUN, ...) {
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
              Corpus(object) <- tolower(object)
              return(object)
          })

setGeneric("strip_whitespace", function(object, ...) standardGeneric("strip_whitespace"))
setMethod("strip_whitespace",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              Corpus(object) <- gsub("[[:space:]]+", " ", object)
              return(object)
          })

setGeneric("stem_doc", function(object, ...) standardGeneric("stem_doc"))
setMethod("stem_doc",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
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
              require(Rstem)
              splittedCorpus <- unlist(strsplit(object, " ", fixed = TRUE))
              noStopwordsCorpus <- splittedCorpus[!splittedCorpus %in% stopwords]
              Corpus(object) <- paste(noStopwordsCorpus, collapse = " ")
              return(object)
          })

setGeneric("tm_filter", function(object, ..., FUN = s_filter, doclevel = FALSE) standardGeneric("tm_filter"))
setMethod("tm_filter",
          signature(object = "TextDocCol"),
          function(object, ..., FUN = s_filter, doclevel = FALSE) {
              if (doclevel)
                  return(object[sapply(object, FUN, ..., DMetaData = DMetaData(object))])
              else
                  return(object[FUN(object, ...)])
          })

setGeneric("tm_index", function(object, ..., FUN = s_filter, doclevel = FALSE) standardGeneric("tm_index"))
setMethod("tm_index",
          signature(object = "TextDocCol"),
          function(object, ..., FUN = s_filter, doclevel = FALSE) {
              if (doclevel)
                  return(sapply(object, FUN, ..., DMetaData = DMetaData(object)))
              else
                  return(FUN(object, ...))
          })

s_filter <- function(object, s, ...) {
    query.df <- DMetaData(object)
    con <- textConnection(s)
    tokens <- scan(con, "character")
    close(con)
    local.meta <- lapply(object, LocalMetaData)
    local.used.meta <- lapply(local.meta, function(x) names(x) %in% tokens)
    l.meta <- NULL
    for (i in 1:length(object)) {
        l.meta <- c(l.meta, list(local.meta[[i]][local.used.meta[[i]]]))
    }
    # Load local meta data from text documents into data frame
    for (i in 1:length(l.meta)) {
        l.meta[[i]] <- c(l.meta[[i]], list(author = Author(object[[i]])))
        l.meta[[i]] <- c(l.meta[[i]], list(datetimestamp = DateTimeStamp(object[[i]])))
        l.meta[[i]] <- c(l.meta[[i]], list(description = Description(object[[i]])))
        l.meta[[i]] <- c(l.meta[[i]], list(identifier = ID(object[[i]])))
        l.meta[[i]] <- c(l.meta[[i]], list(origin = Origin(object[[i]])))
        l.meta[[i]] <- c(l.meta[[i]], list(heading = Heading(object[[i]])))
    }
    for (i in 1:length(l.meta)) {
        for (j in 1:length(l.meta[[i]])) {
            m <- l.meta[[i]][[j]]
            m.name <- names(l.meta[[i]][j])
            if (!(m.name %in% names(query.df))) {
                before <- rep(NA, i - 1)
                after <- rep(NA, length(l.meta) - i)
                if (length(m) > 1) {
                    nl <- vector("list", length(l.meta))
                    nl[1:(i-1)] <- before
                    nl[i] <- list(m)
                    nl[(i+1):length(l.meta)] <- after
                    insert <- data.frame(I(nl), stringsAsFactors = FALSE)
                }
                else
                    insert <- c(before, m, after)
                query.df <- cbind(query.df, insert, stringsAsFactors = FALSE)
                names(query.df)[length(query.df)] <- m.name
            }
            else {
                if (is.null(m))
                    m <- NA
                if (length(m) > 1) {
                    rl <- query.df[ , m.name]
                    rl[i] <- list(m)
                    query.df[ , m.name] <- data.frame(I(rl), stringsAsFactors = FALSE)
                }
                else
                    query.df[i, m.name] <- m
            }
        }
    }
    attach(query.df)
    try(result <- rownames(query.df) %in% row.names(query.df[eval(parse(text = s)), ]))
    detach(query.df)
    return(result)
}

setGeneric("search_fulltext", function(object, pattern, ...) standardGeneric("search_fulltext"))
setMethod("search_fulltext",
          signature(object = "PlainTextDocument", pattern = "character"),
          function(object, pattern, ...) {
              return(any(grep(pattern, Corpus(object))))
          })

setGeneric("append_elem", function(object, data, meta = NULL) standardGeneric("append_elem"))
setMethod("append_elem",
          signature(object = "TextDocCol", data = "TextDocument"),
          function(object, data, meta = NULL) {
              object@.Data[[length(object)+1]] <- data
              object@DMetaData <- rbind(object@DMetaData, c(MetaID = DCMetaData(object)@NodeID, meta))
              return(object)
          })

setGeneric("append_meta", function(object, dcmeta = NULL, dmeta = NULL) standardGeneric("append_meta"))
setMethod("append_meta",
          signature(object = "TextDocCol"),
          function(object, dcmeta = NULL, dmeta = NULL) {
              object@DCMetaData@MetaData <- c(object@DCMetaData@MetaData, dcmeta)
              if (!is.null(dcmeta))
                  object@DMetaData <- cbind(object@DMetaData, dmeta)
              return(object)
          })

setGeneric("remove_meta", function(object, dcname = NULL, dname = NULL) standardGeneric("remove_meta"))
setMethod("remove_meta",
          signature(object = "TextDocCol"),
          function(object, dcname = NULL, dname = NULL) {
              if (!is.null(dcname)) {
                  object@DCMetaData@MetaData <- DCMetaData(object)@MetaData[names(DCMetaData(object)@MetaData) != dcname]
              }
              if (!is.null(dname)) {
                  object@DMetaData <- DMetaData(object)[names(DMetaData(object)) != dname]
              }
              return(object)
          })

setGeneric("prescind_meta", function(object, meta) standardGeneric("prescind_meta"))
setMethod("prescind_meta",
          signature(object = "TextDocCol", meta = "character"),
          function(object, meta) {
              for (m in meta) {
                  if (m %in% c("Author", "DateTimeStamp", "Description", "ID", "Origin", "Heading")) {
                      local.m <- lapply(object, m)
                      local.m <- lapply(local.m, function(x) if (is.null(x)) return(NA) else return(x))
                      local.m <- unlist(local.m)
                      object@DMetaData <- cbind(DMetaData(object), data.frame(m = local.m), stringsAsFactors = FALSE)
                      names(object@DMetaData)[length(object@DMetaData)] <- m
                  }
                  else {
                      local.meta <- lapply(object, LocalMetaData)
                      local.m <- lapply(local.meta, "[[", m)
                      local.m <- lapply(local.m, function(x) if (is.null(x)) return(NA) else return(x))
                      if (length(local.m) == length(unlist(local.m)))
                          local.m <- unlist(local.m)
                      else
                          local.m <- I(local.m)
                      object@DMetaData <- cbind(DMetaData(object), data.frame(m = local.m), stringsAsFactors = FALSE)
                      names(object@DMetaData)[length(object@DMetaData)] <- m
                  }
              }
              return(object)
          })

setMethod("[",
          signature(x = "TextDocCol", i = "ANY", j = "ANY", drop = "ANY"),
          function(x, i, j, ... , drop) {
              if(missing(i))
                  return(x)

              object <- x
              object@.Data <- x@.Data[i, ..., drop = FALSE]
              df <- as.data.frame(DMetaData(object)[i, ])
              names(df) <- names(DMetaData(object))
              object@DMetaData <- df
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
              return(load_doc(x@.Data[[i]]))
          })

setMethod("[[<-",
          signature(x = "TextDocCol", i = "ANY", j = "ANY", value = "ANY"),
          function(x, i, j, ..., value) {
              object <- x
              object@.Data[[i, ...]] <- value
              return(object)
          })

# Update \code{NodeID}s of a DCMetaData tree
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
          function(x, ..., meta = list(merge_date = Sys.time(), merger = Sys.getenv("LOGNAME")), recursive = TRUE) {
              args <- list(...)
              if(length(args) == 0)
                  return(x)

              result <- x
              for (c in args) {
                  if (!inherits(c, "TextDocCol"))
                      stop("invalid argument")
                  result <- c2(result, c)
              }
              return(result)
          })

setGeneric("c2", function(x, y, ..., meta = list(merge_date = Sys.time(), merger = Sys.getenv("LOGNAME")), recursive = TRUE) standardGeneric("c2"))
setMethod("c2",
          signature(x = "TextDocCol", y = "TextDocCol"),
          function(x, y, ..., meta = list(merge_date = Sys.time(), merger = Sys.getenv("LOGNAME")), recursive = TRUE) {
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


setMethod("c",
          signature(x = "TextDocument"),
          function(x, ..., recursive = TRUE){
              args <- list(...)
              if(length(args) == 0)
                  return(x)

              dmeta.df <- data.frame(MetaID = rep(0, length(list(x, ...))), stringsAsFactors = FALSE)
              dcmeta.node <- new("MetaDataNode",
                            NodeID = 0,
                            MetaData = list(create_date = Sys.time(), creator = Sys.getenv("LOGNAME")),
                            children = list())

              return(new("TextDocCol", .Data = list(x, ...), DMetaData = dmeta.df, DCMetaData = dcmeta.node))
          })

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
                  cat(sprintf(ngettext(length(DCMetaData(object)@MetaData),
                                              "\nThe metadata consists of %d tag-value pair and a data frame\n",
                                              "\nThe metadata consists of %d tag-value pairs and a data frame\n"),
                                       length(DCMetaData(object)@MetaData)))
                  cat("Available tags are:\n")
                  cat(names(DCMetaData(object)@MetaData), "\n")
                  cat("Available variables in the data frame are:\n")
                  cat(names(DMetaData(object)), "\n")
              }
    })

setGeneric("inspect", function(object) standardGeneric("inspect"))
setMethod("inspect",
          signature("TextDocCol"),
          function(object) {
              summary(object)
              cat("\n")
              show(object@.Data)
          })

# No metadata is checked
setGeneric("%IN%", function(x, y) standardGeneric("%IN%"))
setMethod("%IN%",
          signature(x = "TextDocument", y = "TextDocCol"),
          function(x, y) {
              x %in% y
          })
