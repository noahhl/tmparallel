# Author: Ingo Feinerer

setGeneric("dissimilarity", function(x, y, method) standardGeneric("dissimilarity"))
setMethod("dissimilarity",
          signature(x = "TermDocMatrix", y = "ANY", method = "character"),
          function(x, y = NULL, method) {
              # Until factored out in a seperate package
              # use the \code{dists} function from the \pkg{cba} package
              dists(x, y, method)
          })
setMethod("dissimilarity",
          c("TextDocument", "TextDocument", "character"),
          function(x, y, method) {
              tdm <- TermDocMatrix(as(list(x,y), "TextDocCol"))
              dissim <- dissimilarity(tdm, method = method)
              class(dissim) <- "dist"
              return(dissim)
          })
