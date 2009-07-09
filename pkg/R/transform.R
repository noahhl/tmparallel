# Author: Ingo Feinerer
# Transformations

tmReduce <- function(x, tmFuns, ...)
    Reduce(function(f, ...) f(...), tmFuns, x, right = TRUE)

getTransformations <- function()
    c("asPlain", "convert_UTF_8", "removeNumbers",
    "removePunctuation", "removeWords", "replacePatterns", "stemDoc",
    "stripWhitespace", "tmTolower")

convert_UTF_8 <- function(x, from = "", sub = NA, ...) {
    Content(x) <- iconv(x, from = from, to = "UTF-8", sub = sub)
    x
}

setGeneric("removeNumbers", function(object, ...) standardGeneric("removeNumbers"))
.removeNumbers <- function(object, ...) {
    Content(object) <- gsub("[[:digit:]]+", "", object)
    object
}
setMethod("removeNumbers", signature(object = "PlainTextDocument"), .removeNumbers)
#setMethod("removeNumbers", signature(object = "MinimalDocument"), .removeNumbers)

setGeneric("removePunctuation", function(object, ...) standardGeneric("removePunctuation"))
.removePunctuation <- function(object, ...) {
    Content(object) <- gsub("[[:punct:]]+", "", Content(object))
    object
}
setMethod("removePunctuation", signature(object = "PlainTextDocument"), .removePunctuation)
#setMethod("removePunctuation", signature(object = "MinimalDocument"), .removePunctuation)

setGeneric("removeWords", function(object, words, ...) standardGeneric("removeWords"))
.removeWords <- function(object, words, ...) {
    Content(object) <- gsub(paste("([[:blank:]]|^)",
                                  paste(words, collapse = "([[:blank:]]|$)|([[:blank:]]|^)"),
                                  "([[:blank:]]|$)", sep = ""),
                            " ",
                            # Add blank so that adjacent words can be matched
                            gsub("([[:blank:]])", "\\1 ", Content(object)))
    # Remove doubled blanks
    Content(object) <- gsub("([[:blank:]]) ", "\\1", Content(object))
    object
}
setMethod("removeWords", signature(object = "PlainTextDocument", words = "character"), .removeWords)
#setMethod("removeWords", signature(object = "MinimalDocument", words = "character"), .removeWords)

setGeneric("replacePatterns", function(object, patterns, by, ...) standardGeneric("replacePatterns"))
.replacePatterns <- function(object, patterns, by, ...) {
    Content(object) <- gsub(patterns, by, Content(object))
    object
}
setMethod("replacePatterns", signature(object = "PlainTextDocument", patterns = "character", by = "character"), .replacePatterns)
#setMethod("replacePatterns", signature(object = "MinimalDocument", patterns = "character", by = "character"), .replacePatterns)

setGeneric("stemDoc", function(object, language = "english", ...) standardGeneric("stemDoc"))
.stemDoc <- function(object, language = "english", ...) {
    stemLine <- function(x) Snowball::SnowballStemmer(x, RWeka::Weka_control(S = language))
    s <- sapply(object,
                function(x) paste(stemLine(unlist(strsplit(x, "[[:blank:]]"))), collapse = " "),
                USE.NAMES = FALSE)
    Content(object) <- if (is.character(s)) s else ""
    object
}
setMethod("stemDoc", signature(object = "PlainTextDocument"), .stemDoc)
#setMethod("stemDoc", signature(object = "MinimalDocument"), .stemDoc)

setGeneric("stripWhitespace", function(object, ...) standardGeneric("stripWhitespace"))
.stripWhitespace <- function(object, ...) {
    Content(object) <- gsub("[[:space:]]+", " ", object)
    object
}
setMethod("stripWhitespace", signature(object = "PlainTextDocument"), .stripWhitespace)
#setMethod("stripWhitespace", signature(object = "MinimalDocument"), .stripWhitespace)

setGeneric("tmTolower", function(object, ...) standardGeneric("tmTolower"))
.tmTolower <- function(object, ...) {
    Content(object) <- tolower(object)
    object
}
setMethod("tmTolower", signature(object = "PlainTextDocument"), .tmTolower)
#setMethod("tmTolower", signature(object = "MinimalDocument"), .tmTolower)
