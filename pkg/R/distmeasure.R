# Author: Ingo Feinerer

setGeneric("dissimilarity", function(x, y = NULL, method) standardGeneric("dissimilarity"))
setMethod("dissimilarity",
          signature(x = "TermDocumentMatrix", y = "ANY", method = "character"),
          function(x, y = NULL, method) {
              require("proxy")
              proxy::dist(as(x, "matrix"), y, method)
          })
setMethod("dissimilarity",
          signature(x = "TextDocument", y = "TextDocument", method = "character"),
          function(x, y = NULL, method) {
              tdm <- DocumentTermMatrix(c(x, y))
              dissim <- dissimilarity(tdm, method = method)
              return(dissim)
          })
