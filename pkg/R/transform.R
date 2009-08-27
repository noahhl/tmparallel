# Author: Ingo Feinerer
# Transformations

tm_map <- function(x, FUN, ..., lazy = FALSE) UseMethod("tm_map", x)
tm_map.VCorpus <- function(x, FUN, ..., lazy = FALSE) {
    result <- x
    # Lazy mapping
    if (lazy) {
        lazyTmMap <- meta(x, tag = "lazyTmMap", type = "corpus")
        if (is.null(lazyTmMap)) {
            meta(result, tag = "lazyTmMap", type = "corpus") <-
                list(index = rep(TRUE, length(result)),
                     maps = list(function(x, DMetaData) FUN(x, ..., DMetaData = DMetaData)))
        }
        else {
            lazyTmMap$maps <- c(lazyTmMap$maps, list(function(x, DMetaData) FUN(x, ..., DMetaData = DMetaData)))
            meta(result, tag = "lazyTmMap", type = "corpus") <- lazyTmMap
        }
    }
    else {
        Content(result) <- if (clusterAvailable())
            snow::parLapply(snow::getMPIcluster(), x, FUN, ..., DMetaData = DMetaData(x))
        else
            lapply(x, FUN, ..., DMetaData = DMetaData(x))
    }
    result
}
tm_map.PCorpus <- function(x, FUN, ..., lazy = FALSE) {
    if (lazy)
        warning("lazy mapping is deactived when using database backend")
    db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
    i <- 1
    for (id in unlist(x)) {
        db[[id]] <- FUN(x[[i]], ..., DMetaData = DMetaData(x))
        i <- i + 1
    }
    # Suggested by Christian Buchta
    filehash::dbReorganize(db)

    x
}

# Materialize lazy mappings
# Improvements by Christian Buchta
# TODO: Fix S4 usage (both in R as in C)
materialize <- function(corpus, range = seq_along(corpus)) {
    lazyTmMap <- meta(corpus, tag = "lazyTmMap", type = "corpus")
    if (!is.null(lazyTmMap)) {
       # Make valid and lazy index
       idx <- (seq_along(corpus) %in% range) & lazyTmMap$index
       if (any(idx)) {
           res <- corpus@.Data[idx]
           for (m in lazyTmMap$maps)
               res <- lapply(res, m, DMetaData = DMetaData(corpus))
           corpus@.Data[idx] <- res
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
    c("as.PlainTextDocument", "convert_UTF_8", "removeNumbers", "removePunctuation",
      "removeWords", "stemDocument", "stripWhitespace", "tm_tolower")

as.PlainTextDocument <- function(x, FUN, ...) UseMethod("as.PlainTextDocument", x)
as.PlainTextDocument.RCV1Document <- function(x, FUN, ...) {
    Content(x) <- unlist(XML::xmlApply(XML::xmlRoot(x)[["text"]], XML::xmlValue), use.names = FALSE)
    class(x) <- c("PlainTextDocument", "TextDocument", "character")
    x
}
as.PlainTextDocument.Reuters21578Document <- function(x, FUN, ...) {
    Content(x) <- unlist(XML::xmlApply(XML::xmlRoot(x)[["TEXT"]], XML::xmlValue), use.names = FALSE)
    class(x) <- c("PlainTextDocument", "TextDocument", "character")
    x
}

convert_UTF_8 <- function(x, from = "", sub = NA, ...)
    iconv(x, from = from, to = "UTF-8", sub = sub)

removeNumbers <- function(x, ...) UseMethod("removeNumbers", x)
removeNumbers.PlainTextDocument <- function(x, ...) gsub("[[:digit:]]+", "", x)

removePunctuation <- function(x, ...) UseMethod("removePunctuation", x)
removePunctuation.PlainTextDocument <- function(x, ...)  gsub("[[:punct:]]+", "", x)

removeWords <- function(x, words, ...) UseMethod("removeWords", x)
removeWords.PlainTextDocument <- function(x, words, ...) {
    x <- gsub(paste("([[:blank:]]|^)",
                    paste(words, collapse = "([[:blank:]]|$)|([[:blank:]]|^)"),
                    "([[:blank:]]|$)", sep = ""),
              " ",
              # Add blank so that adjacent words can be matched
              gsub("([[:blank:]])", "\\1 ", x))
    # Remove doubled blanks
    gsub("([[:blank:]]) ", "\\1", x)
}

stemDocument <- function(x, language = "english", ...) UseMethod("stemDocument", x)
stemDocument.PlainTextDocument <- function(x, language = "english", ...) {
    stemLine <- function(x) Snowball::SnowballStemmer(x, RWeka::Weka_control(S = language))
    s <- unlist(lapply(x, function(x) paste(stemLine(unlist(strsplit(x, "[[:blank:]]"))), collapse = " ")))
    Content(x) <- if (is.character(s)) s else ""
    x
}

stripWhitespace <- function(x, ...) UseMethod("stripWhitespace", x)
stripWhitespace.PlainTextDocument <- function(x, ...)  gsub("[[:space:]]+", " ", x)

tm_tolower <- function(x, ...) UseMethod("tm_tolower", x)
tm_tolower.PlainTextDocument <- function(x, ...) tolower(x)
