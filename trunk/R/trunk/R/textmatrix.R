### This preprocessing code was adapted from the
### R package "lsa" written by Fridolin Wild.

### dependencies: library("RStem")

textvector <- function(file, stemming = FALSE, language = "german", minWordLength = 3, minDocFreq = 1, stopwords = NULL) {

    txt = scan(file, what = "character", quiet = TRUE)
    txt = gsub( "\\.|:|\\(|\\)|\\[|\\]|\\{|\\}|,|;|\\?|-|\\!|\"|\'|\`|\\^|\/", " ", txt)
    txt = gsub("[[:space:]]+", " ", txt)
    txt = tolower(txt)
    txt = unlist(strsplit(txt, " ", fixed=TRUE))

    # stopword filtering?
    if (!is.null(stopwords)) txt = txt[!txt %in% stopwords]

    # tabulate
    tab = sort(table(txt), decreasing = TRUE)

    # with threshold minDocFreq
    tab = tab[tab >= minDocFreq]

    # wordLength filtering?
    tab = tab[nchar(names(tab)) > minWordLength]

    # stemming?
    if (stemming) names(tab) = wordStem(names(tab), language=language)

    return(data.frame(docs = basename(file), terms = names(tab), Freq = tab, row.names = NULL))
}

textmatrix <- function(directory, stemming = FALSE, language = "german", minWordLength = 3, minDocFreq = 1, stopwords = NULL) {

    dummy = lapply(dir(directory, full.names = TRUE), textvector, stemming, language, minWordLength, minDocFreq, stopwords)
    tm = xtabs(Freq ~ ., data = do.call("rbind", dummy))
    class(tm) = "textmatrix"

    return(tm)
}
