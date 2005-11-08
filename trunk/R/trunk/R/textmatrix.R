# Author: Ingo Feinerer
# The preprocessing code was adapted from the R package "lsa" written by Fridolin Wild.

# S4 class definition
# Term-document matrix
setClass("termdocmatrix", representation(mat = "matrix"))

# Accessor function
if (!isGeneric("mat")) {
    if (is.function("mat"))
        fun <- mat
    else
        fun <- function(object) standardGeneric("mat")
    setGeneric("mat", fun)
}
setMethod("mat", "termdocmatrix", function(object) object@mat)

setGeneric("termdocmatrix", function(object) standardGeneric("termdocmatrix"))
setMethod("termdocmatrix", "list", function(object) {
    new("termdocmatrix", mat = textmatrix(object))
})

textvector <- function(doc, stemming = FALSE, language = "german", minWordLength = 3, minDocFreq = 1, stopwords = NULL) {
    txt <- corpus(doc)
    txt <- gsub( "\\.|:|\\(|\\)|\\[|\\]|\\{|\\}|,|;|\\?|-|\\!|\"|\'|\`|\\^|\/", " ", txt)
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
    tab <- tab[nchar(names(tab)) > minWordLength]

    # stemming?
    if (stemming) {
        require(Rstem)
        names(tab) <- wordStem(names(tab), language = language)
    }

    data.frame(docs = id(doc), terms = names(tab), Freq = tab, row.names = NULL)
}

textmatrix <- function(docs, stemming = FALSE, language = "german", minWordLength = 3, minDocFreq = 1, stopwords = NULL) {
    tvlist <- lapply(docs, textvector, stemming, language, minWordLength, minDocFreq, stopwords)
    tm <- as.matrix(xtabs(Freq ~ ., data = do.call("rbind", tvlist)))
    class(tm) <- "matrix"
    tm
}
