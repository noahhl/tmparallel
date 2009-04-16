# Author: Ingo Feinerer

TermDocMatrix <- function(object, control = list()) {
    .Defunct("DocumentTermMatrix", package = "tm")
}

setGeneric("TermDocumentMatrix", function(object, control = list()) standardGeneric("TermDocumentMatrix"))
setMethod("TermDocumentMatrix",
          signature(object = "Corpus"),
          function(object, control = list()) {

              weight <- control$weighting
              if (is.null(weight))
                  weight <- weightTf

              tflist <- if (clusterAvailable())
                  snow::parLapply(snow::getMPIcluster(), object, termFreq, control)
              else
                  lapply(object, termFreq, control)

              tflist <- lapply(tflist, function(x) x[x > 0])
              allTerms <- sort(unique(unlist(lapply(tflist, names), use.names = FALSE)))

              i <- lapply(tflist, function(x) match(names(x), allTerms))
              p <- c(0L, cumsum(sapply(i, length)))
              i <- unlist(i) - 1L

              x <- as.numeric(unlist(tflist, use.names = FALSE))
              rm(tflist)

              tdm <- new("TermDocumentMatrix", p = p, i = i, x = x,
                         Dim = c(length(allTerms), length(p) - 1L),
                         Transpose = FALSE,
                         Weighting = c(weight@Name, weight@Acronym))
              tdm <- weight(tdm)
              tdm@Dimnames <- list(Terms = allTerms, Docs = sapply(object, ID))

              tdm
          })

DocumentTermMatrix <- function(object, control = list()) {
    m <- TermDocumentMatrix(object, control)
    m@Transpose <- TRUE
    m
}

termFreq <- function(doc, control = list()) {
    txt <- Content(doc)

    # Conversion to lower characters
    tolower <- control$tolower
    if (is.null(tolower) || tolower)
        txt <- tolower(txt)

    # Tokenize the corpus
    tokenize <- control$tokenize
    if (is.null(tokenize))
        tokenize <- function(x) unlist(strsplit(gsub("[^[:alnum:]]+", " ", x), " ", fixed = TRUE))
    txt <- tokenize(txt)

    # Number removal
    if (isTRUE(control$removeNumbers))
        txt <- gsub("[[:digit:]]+", "", txt)

    # Stemming
    if (isTRUE(control$stemming))
        txt <- Snowball::SnowballStemmer(txt, RWeka::Weka_control(S = resolveISOCode(Language(doc))))

    # Stopword filtering
    stopwords <- control$stopwords
    if (isTRUE(stopwords))
        txt <- txt[!txt %in% stopwords(Language(doc))]
    else if (is.character(stopwords))
        txt <- txt[!txt %in% stopwords]

    # Check if the document content is NULL
    if (is.null(txt))
        return(structure(integer(0), names = character(0)))

    # If dictionary is set tabulate against it
    dictionary <- control$dictionary
    tab <-  if (is.null(dictionary))
        table(txt)
    else
        table(factor(txt, levels = dictionary))

    # Ensure minimum document frequency threshold
    minDocFreq <- control$minDocFreq
    if (!is.null(minDocFreq))
        tab <- tab[tab >= minDocFreq]

    # Filter out too short terms
    minWordLength <- control$minWordLength
    if (is.null(minWordLength))
        minWordLength <- 3
    tab <- tab[nchar(names(tab), type = "chars") >= minWordLength]

    # Return named integer
    structure(as.integer(tab), names = names(tab))
}

setMethod("show",
          signature(object = "TermDocumentMatrix"),
          function(object){
              type <- if (object@Transpose) "document-term matrix" else "term-document matrix"
              cat(sprintf("A %s\n", type), "\n")
              m <- if(object@Transpose) t(object) else object
              show(as(m, "dgCMatrix"))
              cat(sprintf("\nWeighting: %s (%s)\n", object@Weighting[1], object@Weighting[2]))
    })

setMethod("summary",
          signature(object = "TermDocumentMatrix"),
          function(object){
              show(object)
              cat(sprintf("\nNon-/sparse entries: %d/%d\n", length(object@x), prod(dim(object)) - length(object@x)))
              cat(sprintf("Sparsity           : %d%%\n", round((1 - length(object@x)/prod(dim(object))) * 100)))
              cat("Maximal term length:", max(nchar(Terms(object), type = "chars")), "\n")
          })

subsetTermDocumentMatrix <- function(x, i, j, ..., drop) {
    # Swap indices for DocumentTermMatrix
    if (x@Transpose) {
        k <- i
        i <- j
        j <- k
    }
    dgCMatrix <- as(x, "dgCMatrix")
    if (identical(i, NA)) i <- seq_len(nrow(dgCMatrix))
    if (identical(j, NA)) j <- seq_len(ncol(dgCMatrix))
    dgCMatrix <- dgCMatrix[i, j, ..., drop = FALSE]
    for (s in slotNames(dgCMatrix))
        slot(x, s) <- slot(dgCMatrix, s)
    x
}

setMethod("[", signature(x = "TermDocumentMatrix", i = "index", j = "missing", drop = "missing"),
          function(x, i, j, ..., drop) subsetTermDocumentMatrix(x, i, NA, ..., drop))
setMethod("[", signature(x = "TermDocumentMatrix", i = "missing", j = "index", drop = "missing"),
          function(x, i, j, ..., drop) subsetTermDocumentMatrix(x, NA, j, ..., drop))
setMethod("[", signature(x = "TermDocumentMatrix", i = "index", j = "index", drop = "missing"),
          function(x, i, j, ..., drop) subsetTermDocumentMatrix(x, i, j, ..., drop))

setMethod("dim",
          signature(x = "TermDocumentMatrix"),
          function(x) {
              dim <- dim(as(x, "dgCMatrix"))
              if (x@Transpose) rev(dim) else dim
          })

setMethod("ncol",
          signature(x = "TermDocumentMatrix"),
          function(x) {
              m <- as(x, "dgCMatrix")
              if (x@Transpose) nrow(m) else ncol(m)
          })

setMethod("nrow",
          signature(x = "TermDocumentMatrix"),
          function(x) {
              m <- as(x, "dgCMatrix")
              if (x@Transpose) ncol(m) else nrow(m)
          })

nDocs <- function(x) x@Dim[[2]]
nTerms <- function(x) x@Dim[[1]]

setMethod("dimnames",
          signature(x = "TermDocumentMatrix"),
          function(x) {
              dimNames <- dimnames(as(x, "dgCMatrix"))
              if (x@Transpose) rev(dimNames) else dimNames
          })

setMethod("colnames",
          signature(x = "TermDocumentMatrix"),
          function(x, do.NULL = TRUE, prefix = "col") {
              m <- as(x, "dgCMatrix")
              if (x@Transpose) rownames(m) else colnames(m)
          })

setMethod("rownames",
          signature(x = "TermDocumentMatrix"),
          function(x, do.NULL = TRUE, prefix = "row") {
              m <- as(x, "dgCMatrix")
              if (x@Transpose) colnames(m) else rownames(m)
          })

Docs <- function(x) x@Dimnames[[2]]
Terms <- function(x) x@Dimnames[[1]]

setMethod("as.matrix",
          signature(x = "TermDocumentMatrix"),
          function(x) {
              m <- as(x, "dgCMatrix")
              m <- if(x@Transpose) t(as.matrix(m)) else as.matrix(m)
          })

setGeneric("findFreqTerms", function(object, lowfreq = 0, highfreq = Inf) standardGeneric("findFreqTerms"))
setMethod("findFreqTerms",
          signature(object = "TermDocumentMatrix"),
          function(object, lowfreq = 0, highfreq = Inf) {
              m <- as(as(object, "dgCMatrix"), "TsparseMatrix")
              Terms(object)[unique(m@i[m@x >= lowfreq & m@x <= highfreq]) + 1]
          })

setGeneric("findAssocs", function(object, term, corlimit) standardGeneric("findAssocs"))
setMethod("findAssocs",
          signature(object = "TermDocumentMatrix", term = "character"),
          function(object, term, corlimit) {
              suppressWarnings(object.cor <- cor(as.matrix(as(t(object), "dgCMatrix"))))
              findAssocs(object.cor, term, corlimit)
          })
setMethod("findAssocs",
          signature(object = "matrix", term = "character"),
          function(object, term, corlimit) {
              sort(round(object[term, which(object[term,] > corlimit)], 2), decreasing = TRUE)
          })

setGeneric("removeSparseTerms", function(object, sparse) standardGeneric("removeSparseTerms"))
setMethod("removeSparseTerms",
          signature(object = "TermDocumentMatrix", sparse = "numeric"),
          function(object, sparse) {
              if ((sparse <= 0) || (sparse >= 1))
                  stop("invalid sparse factor")
              else {
                  m <- as(as(object, "dgCMatrix"), "TsparseMatrix")
                  t <- table(m@i + 1) > ncol(m) * (1 - sparse)
                  m <- as(m[as.numeric(names(t[t])),], "dgCMatrix")
                  for (s in slotNames(m))
                      slot(object, s) <- slot(m, s)
                  object
              }
          })
