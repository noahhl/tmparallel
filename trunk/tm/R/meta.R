# Author: Ingo Feinerer

setGeneric("meta", function(object, tag = NULL, type = NULL) standardGeneric("meta"))
setMethod("meta",
          signature(object = "Corpus"),
          function(object, tag = NULL, type = "indexed") {
              if ((type != "indexed") && (type != "corpus") && (type != "local"))
                  stop("invalid type")
              if (is.null(tag) && type == "indexed")
                  return(DMetaData(object))
              if (is.null(tag) && type == "corpus")
                  return(CMetaData(object))
              if (is.null(tag) && type == "local")
                  return(invisible(sapply(object, meta)))
              if (type == "indexed")
                  return(DMetaData(object)[tag])
              if (type == "local")
                  return(slot(tag, object))
              else # (type == "corpus")
                  return(CMetaData(object)@MetaData[[tag]])
          })
setMethod("meta",
          signature(object = "TextRepository"),
          function(object, tag = NULL, type = NULL) {
              if (is.null(tag))
                  RepoMetaData(object)
              else
                  RepoMetaData(object)[[tag]]
          })
setMethod("meta",
          signature(object = "TextDocument"),
          function(object, tag = NULL, type = NULL) {
              if (is.null(tag)) {
                  slots <- sort(setdiff(names(getSlots(class(object))), c(".Data", "LocalMetaData")))
                  cat("Available meta data pairs are:\n")
                  for (s in slots)
                      cat(sprintf("  %-13s: %s\n", s, paste(as(slot(object, s), "character"), collapse = " ")))
                  cat("Dynamic local meta data pairs are:\n")
                  show(LocalMetaData(object))
              }
              else
                  LocalMetaData(object)[[tag]]
          })

setGeneric("meta<-", function(object, tag, type = NULL, value) standardGeneric("meta<-"))
setReplaceMethod("meta",
                 signature(object = "Corpus"),
                 function(object, tag, type = "indexed", value) {
                     if ((type != "indexed") && (type != "corpus") && (type != "local"))
                         stop("invalid type")
                     if (type == "indexed")
                         DMetaData(object)[, tag] <- value
                     else if (type == "local")
                         for (i in seq_along(object))
                             meta(object[[i]], tag) <- value[[i]]
                     else # (type == "corpus")
                         object@CMetaData@MetaData[[tag]] <- value
                     object
                 })
setReplaceMethod("meta",
                 signature(object = "TextRepository"),
                 function(object, tag, type = NULL, value) {
                     object@RepoMetaData[[tag]] <- value
                     object
})
setReplaceMethod("meta",
                 signature(object = "TextDocument"),
                 function(object, tag, type = NULL, value) {
                     if (tag %in% c("Author", "DateTimeStamp", "Description", "ID", "Heading", "Language", "Origin"))
                         slot(object, tag) <- value
                     else
                         object@LocalMetaData[[tag]] <- value
                     object
})
