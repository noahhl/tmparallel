# Author: Ingo Feinerer

setClass("Dictionary",
         contains = "character")

setGeneric("Dictionary", function(object) standardGeneric("Dictionary"))
setMethod("Dictionary",
          signature(object = "character"),
          function(object) {
              new("Dictionary", .Data = object)
          })
setMethod("Dictionary",
          signature(object = "TermDocMatrix"),
          function(object) {
              new("Dictionary", .Data = colnames(Data(object)))
          })