# Author: Ingo Feinerer

Content <- function(x) UseMethod("Content", x)
Content.PlainTextDocument <- function(x) as.character(x)

`Content<-` <- function(x, value) UseMethod("Content<-", x)
`Content<-.default` <- function(x, value) {
    attrs <- attributes(x)
    x <- value
    attributes(x) <- attrs
    x
}
`Content<-.PlainTextDocument` <- function(x, value) {
    attrs <- attributes(x)
    x <- as.character(value)
    attributes(x) <- attrs
    x
}
`Content<-.XMLDocument` <- function(x, value) {
    attrs <- attributes(x)
    x <- value
    attributes(x) <- attrs
    attr(x, "names") <- attr(value, "names")
    x
}

Author <- function(x) attr(x, "Author")
DateTimeStamp <- function(x) attr(x, "DateTimeStamp")
Description <- function(x) attr(x, "Description")
Heading <- function(x) attr(x, "Heading")
ID <- function(x) attr(x, "ID")
Language <- function(x) attr(x, "Language")
LocalMetaData <- function(x) attr(x, "LocalMetaData")
Origin <- function(x) attr(x, "Origin")

DMetaData <- function(x) UseMethod("DMetaData", x)
DMetaData.VCorpus <- function(x) attr(x, "DMetaData")
DMetaData.PCorpus <- function(x) {
    db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
    result <- filehash::dbFetch(db, "DMetaData")
    index <- attr(x, "DMetaData")[[1, "subset"]]
    if (!any(is.na(index)))
        result <- result[index, , drop = FALSE]
    result
}

`DMetaData<-` <- function(x, value) UseMethod("DMetaData<-", x)
`DMetaData<-.VCorpus` <- function(x, value) {
    attr(x, "DMetaData") <- value
    x
}
`DMetaData<-.PCorpus` <- function(x, value) {
    db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
    db[["DMetaData"]] <- value
    attr(x, "DMetaData")[[1, "subset"]] <- NA
    x
}

# CMetaData = *MetaData* describing only the Document *C*ollection itself
CMetaData <- function(x) UseMethod("CMetaData", x)
CMetaData.Corpus <- function(x) attr(x, "CMetaData")

DBControl <- function(x) attr(x, "DBControl")

RepoMetaData <- function(x) attr(x, "RepoMetaData")
