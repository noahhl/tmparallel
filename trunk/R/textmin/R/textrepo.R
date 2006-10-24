# Author: Ingo Feinerer

setGeneric("TextRepository", function(object, metaname = "created", meta = date()) standardGeneric("TextRepository"))
setMethod("TextRepository",
          signature(object = "TextDocCol"),
          function(object, metaname, meta) {
              tr <- new("TextRepository", .Data = list(object), RepresentationMetaData = list(created = meta))
              names(tr@RepresentationMetaData) <- metaname
              return(tr)
          })

setMethod("attachData",
          signature(object = "TextRepository", data = "TextDocCol"),
          function(object, data) {
              object@.Data <- as(c(object@.Data, data), "TextRepository")
              return(object)
          })

setMethod("attachMetaData",
          signature(object = "TextRepository"),
          function(object, name, metadata) {
              object@RepresentationMetaData <- c(object@RepresentationMetaData, new = list(metadata))
              names(object@RepresentationMetaData)[length(names(object@RepresentationMetaData))] <- name
              return(object)
          })

setMethod("length",
          signature(x = "TextRepository"),
          function(x){
              return(length(as(x, "list")))
    })

setMethod("show",
          signature(object = "TextRepository"),
          function(object){
              cat("A text repository with", length(object), "text document collection")
              if (length(object) == 1)
                  cat("\n")
              else
                  cat("s\n")
    })

setMethod("summary",
          signature(object = "TextRepository"),
          function(object){
              show(object)
              if (length(RepresentationMetaData(object)) > 0) {
                  cat("\nThe representation metadata consists of", length(RepresentationMetaData(object)), "tag-value pair")
                  if (length(RepresentationMetaData(object)) == 1)
                      cat(".\n")
                  else
                      cat("s.\n")
                  cat("Available tags are:\n")
                  cat(names(RepresentationMetaData(object)), "\n")
              }
    })
