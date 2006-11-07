# Author: Ingo Feinerer

setGeneric("TextRepository", function(object, metaname = "created", meta = date()) standardGeneric("TextRepository"))
setMethod("TextRepository",
          signature(object = "TextDocCol"),
          function(object, metaname, meta) {
              tr <- new("TextRepository", .Data = list(object), RepresentationMetaData = list(created = meta))
              names(tr@RepresentationMetaData) <- metaname
              return(tr)
          })

setMethod("attach_data",
          signature(object = "TextRepository", data = "TextDocCol"),
          function(object, data) {
              object[[length(object)+1]] <- data
              return(object)
          })

setMethod("attach_metadata",
          signature(object = "TextRepository"),
          function(object, name, metadata) {
              object@RepresentationMetaData <- c(object@RepresentationMetaData, new = list(metadata))
              names(object@RepresentationMetaData)[length(names(object@RepresentationMetaData))] <- name
              return(object)
          })

setMethod("remove_metadata",
          signature(object = "TextRepository"),
          function(object, name) {
              object@RepresentationMetaData <- RepresentationMetaData(object)[names(RepresentationMetaData(object)) != name]
              return(object)
          })

setMethod("modify_metadata",
          signature(object = "TextRepository"),
          function(object, name, metadata) {
              object@RepresentationMetaData[[name]] <- metadata
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
               cat(sprintf(ngettext(length(object),
                                    "A text repository with %d text document collection\n",
                                    "A text repository with %d text document collections\n"),
                           length(object)))
    })

setMethod("summary",
          signature(object = "TextRepository"),
          function(object){
              show(object)
              if (length(RepresentationMetaData(object)) > 0) {
                  cat(sprintf(ngettext(length(RepresentationMetaData(object)),
                                              "\nThe representation metadata consists of %d tag-value pair\n",
                                              "\nThe representation metadata consists of %d tag-value pairs\n"),
                                       length(RepresentationMetaData(object))))
                  cat("Available tags are:\n")
                  cat(names(RepresentationMetaData(object)), "\n")
              }
    })
