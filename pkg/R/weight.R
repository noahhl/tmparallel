# Author: Ingo Feinerer

# Weight Function Class
setClass("WeightFunction",
         representation(Name = "character",
                        Acronym = "character"),
         contains = "function")

# Constructor
setGeneric("WeightFunction", function(object, name, acronym) standardGeneric("WeightFunction"))
setMethod("WeightFunction",
          signature(object = "function", name = "character", acronym = "character"),
          function(object, name, acronym) {
              new("WeightFunction", .Data = object, Name = name, Acronym = acronym)
          })

# Actual TermDocumentMatrix weighting functions
weightTf <- WeightFunction(identity, "term frequency", "tf")

weightTfIdf <-
    WeightFunction(function(m) {
        m * log2(ncol(m) / rowSums(weightBin(m)))
    }, "term frequency - inverse document frequency", "tf-idf")

weightBin <-
    WeightFunction(function(m) {
        m@x <- rep(1, length(m@x))
        m
    }, "binary", "bin")
