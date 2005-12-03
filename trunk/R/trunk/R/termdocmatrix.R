# Author: Ingo Feinerer
# The preprocessing code was adapted from the R package "lsa" written by Fridolin Wild.

# S4 class definition
# Term-document matrix
setClass("termdocmatrix",
         representation(weighting = "character"),
         contains = c("matrix"))

# Accessor functions as described in "S4 Classes in 15 pages, more or less"

if (!isGeneric("weighting")) {
    if (is.function("weighting"))
        fun <- weighting
    else
        fun <- function(object) standardGeneric("weighting")
    setGeneric("weighting", fun)
}
setMethod("weighting", "termdocmatrix", function(object) object@weighting)

# Input matrix has to be in term-frequency format
weightMatrix <- function(m, weighting = "tf") {
    type <- match.arg(weighting,c("tf","tf-idf","bin"))
    switch(type,
           "tf" = {
               wm <- m
           },
           "tf-idf" = {
               df <- colSums(((m > 0) * 1))
               wm <- m * log2(nrow(m) / df)
           },
           "bin" = {
               wm <- (m > 0) * 1
           }
           )
    wm
}

setGeneric("termdocmatrix", function(object, ...) standardGeneric("termdocmatrix"))
setMethod("termdocmatrix", c("textdoccol", "character", "logical", "character", "integer", "integer", "character"),
          function(object, weighting = "tf", stemming = FALSE, language = "english",
                   minWordLength = 3, minDocFreq = 1, stopwords = NULL) {
              tvlist <- lapply(object, textvector, stemming, language, minWordLength, minDocFreq, stopwords)
              tm <- as.matrix(xtabs(Freq ~ ., data = do.call("rbind", tvlist)))
              class(tm) <- "matrix"
              tm <- weightMatrix(tm, weighting)

              new("termdocmatrix", .Data = tm, weighting = weighting)
          })

textvector <- function(doc, stemming = FALSE, language = "english", minWordLength = 3, minDocFreq = 1, stopwords = NULL) {
    txt <- gsub( "\\.|:|\\(|\\)|\\[|\\]|\\{|\\}|,|;|\\?|-|\\!|\"|\'|\`|\\^|\=|\’|\–|\„|\”|\/", " ", doc)
    txt <- gsub("[[:space:]]+", " ", txt)
    txt <- tolower(txt)
    txt <- unlist(strsplit(txt, " ", fixed = TRUE))

    # stopword filtering?
    if (!is.null(stopwords)) txt = txt[!txt %in% stopwords]

    # tabulate
    tab <- sort(table(txt), decreasing = TRUE)

    # with threshold minDocFreq
    tab <- tab[tab >= minDocFreq]

    # wordLength filtering?
    tab <- tab[nchar(names(tab), type="chars") >= minWordLength]

    # Is the vector empty?
    if (is.null(names(tab))) {
        terms <- ""
        Freq <- 0
    }
    else {
        # stemming?
        if (stemming) {
            require(Rstem)
            names(tab) <- wordStem(names(tab), language = language)
        }
        terms <- names(tab)
        Freq <- tab
    }

    data.frame(docs = id(doc), terms, Freq, row.names = NULL)
}
