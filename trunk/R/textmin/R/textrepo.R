# Author: Ingo Feinerer

setGeneric("TextRepository", function(object, metaname = "created", meta = date()) standardGeneric("TextRepository"))
setMethod("TextRepository",
          c("TextDocCol"),
          function(object, metaname, meta) {
              tr <- new("TextRepository", .Data = list(object), RepresentationMetaData = list(created = meta))
              names(tr@RepresentationMetaData) <- metaname
              return(tr)
          })


setMethod("attachData",
          c("TextRepository","TextDocCol"),
          function(object, data) {
              object@.Data <- as(c(object@.Data, data), "TextRepository")
              return(object)
          })

setMethod("attachMetaData",
          c("TextRepository"),
          function(object, name, metadata) {
              object@RepresentationMetaData <- c(object@RepresentationMetaData, new = list(metadata))
              names(object@RepresentationMetaData)[length(names(object@RepresentationMetaData))] <- name
              return(object)
          })
