# Author: Ingo Feinerer
# Transformations

tm_map <- function(x, FUN, ..., useMeta = FALSE, lazy = FALSE) UseMethod("tm_map", x)
tm_map.VCorpus <- function(x, FUN, ..., useMeta = FALSE, lazy = FALSE) {
    result <- x
    # Lazy mapping
    if (lazy) {
        lazyTmMap <- meta(x, tag = "lazyTmMap", type = "corpus")
        local_fun <- local({
            useMeta <- useMeta
            function(x, ..., DMetaData) {
                if (useMeta)
                    FUN(x, ..., DMetaData = DMetaData)
                else
                    FUN(x, ...)
            }
        })
        if (is.null(lazyTmMap)) {
            meta(result, tag = "lazyTmMap", type = "corpus") <-
                list(index = rep(TRUE, length(result)), maps = list(local_fun))
        }
        else {
            lazyTmMap$maps <- c(lazyTmMap$maps, list(local_fun))
            meta(result, tag = "lazyTmMap", type = "corpus") <- lazyTmMap
        }
    }
    else {
        Content(result) <- if (clusterAvailable()) {
            if (useMeta)
                snow::parLapply(snow::getMPIcluster(), x, FUN, ..., DMetaData = DMetaData(x))
            else
                snow::parLapply(snow::getMPIcluster(), x, FUN, ...)
        } else {
            if (useMeta)
                llply(.data=x, .fun=FUN, ..., DMetaData = DMetaData(x))
            else
                llply(.data=x, .fun=FUN, ...)
        }
    }
    result
}
tm_map.PCorpus <- function(x, FUN, ..., useMeta = FALSE, lazy = FALSE) {
    if (lazy)
        warning("lazy mapping is deactived when using database backend")
    db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
    i <- 1
    for (id in unlist(x)) {
        db[[id]] <- if (useMeta)
            FUN(x[[i]], ..., DMetaData = DMetaData(x))
        else
            FUN(x[[i]], ...)
        i <- i + 1
    }
    # Suggested by Christian Buchta
    filehash::dbReorganize(db)

    x
}

# Materialize lazy mappings
# Improvements by Christian Buchta
materialize <- function(corpus, range = seq_along(corpus)) {
    lazyTmMap <- meta(corpus, tag = "lazyTmMap", type = "corpus")
    if (!is.null(lazyTmMap)) {
       # Make valid and lazy index
       idx <- (seq_along(corpus) %in% range) & lazyTmMap$index
       if (any(idx)) {
           res <- unclass(corpus)[idx]
           for (m in lazyTmMap$maps)
               res <- lapply(res, m, DMetaData = DMetaData(corpus))
           corpus[idx] <- res
           lazyTmMap$index[idx] <- FALSE
       }
    }
    # Clean up if everything is materialized
    if (!any(lazyTmMap$index))
        lazyTmMap <- NULL
    meta(corpus, tag = "lazyTmMap", type = "corpus") <- lazyTmMap
    corpus
}

tm_reduce <- function(x, tmFuns, ...)
    Reduce(function(f, ...) f(...), tmFuns, x, right = TRUE)

getTransformations <- function()
    c("as.PlainTextDocument", "removeNumbers", "removePunctuation",
      "removeWords", "stemDocument", "stripWhitespace")

as.PlainTextDocument <- function(x) UseMethod("as.PlainTextDocument", x)
as.PlainTextDocument.PlainTextDocument <- identity
as.PlainTextDocument.RCV1Document <- function(x) {
    Content(x) <- unlist(XML::xmlApply(XML::xmlRoot(x)[["text"]], XML::xmlValue), use.names = FALSE)
    class(x) <- c("PlainTextDocument", "TextDocument", "character")
    x
}
as.PlainTextDocument.Reuters21578Document <- function(x) {
    Content(x) <- unlist(XML::xmlApply(XML::xmlRoot(x)[["TEXT"]], XML::xmlValue), use.names = FALSE)
    class(x) <- c("PlainTextDocument", "TextDocument", "character")
    x
}

removeNumbers <- function(x) UseMethod("removeNumbers", x)
removeNumbers.PlainTextDocument <- function(x) gsub("[[:digit:]]+", "", x)

removePunctuation <- function(x) UseMethod("removePunctuation", x)
removePunctuation.PlainTextDocument <- function(x) gsub("[[:punct:]]+", "", x)

## <NOTYET>
## removePunctuation <-
## function(x, preserve_intra_word_dashes = FALSE)
## {
##     if(!preserve_intra_word_dashes)
##         gsub("[[:punct:]]+", "", x)
##     else {
##         ## Assume there are no ASCII 1 characters.
##         x <- gsub("(\\w)-(\\w)", "\\1\1\\2", x)
##         x <- gsub("[[:punct:]]+", "", x)
##         gsub("\1", "-", x, fixed = TRUE)
##     }
## }
## </NOTYET>

removeWords <- function(x, words) UseMethod("removeWords", x)
# Improvements by Kurt Hornik
removeWords.PlainTextDocument <- function(x, words)
    gsub(sprintf("\\b(%s)\\b", paste(words, collapse = "|")), "", x)

stemDocument <- function(x, language = "english", stemmer="Snowball", ...) UseMethod("stemDocument", x)
stemDocument.character <- function(x, language = "english", stemmer="Snowball", ...) {
    if(stemmer == "Snowball") {
      Snowball::SnowballStemmer(x, RWeka::Weka_control(S = language))        
    } else if (stemmer == "Rstem") {
      library("Rstem")
      llply(.data=x, .fun=function(d) {paste(wordStem(substr(strsplit(as.character(d), " ")[[1]], 1, 254)), collapse=" ")}, ...)
    }

}

stemDocument.PlainTextDocument <- function(x, language = map_IETF(Language(x)), stemmer="Snowball", ...) {
    s <- unlist(lapply(x, function(x) paste(stemDocument.character(unlist(strsplit(x, "[[:blank:]]")), language, stemmer, ...), collapse = " ")))
    Content(x) <- if (is.character(s)) s else ""
    x
}

stripWhitespace <- function(x) UseMethod("stripWhitespace", x)
stripWhitespace.PlainTextDocument <- function(x) gsub("[[:space:]]+", " ", x)
