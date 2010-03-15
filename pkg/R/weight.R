# Author: Ingo Feinerer

WeightFunction <- function(x, name, acronym) {
    class(x) <- c("WeightFunction", "function")
    attr(x, "Name") <- name
    attr(x, "Acronym") <- acronym
    x
}

# Actual TermDocumentMatrix weighting functions
weightTf <- WeightFunction(identity, "term frequency", "tf")

weightTfIdf <-
    WeightFunction(function(m, normalize = TRUE) {
        isDTM <- inherits(m, "DocumentTermMatrix")
        if (isDTM) m <- t(m)
        if (normalize) {
            cs <- col_sums(m)
            if (any(cs == 0))
                warning("empty document")
            names(cs) <- seq_len(nDocs(m))
            m$v <- m$v / cs[m$j]
        }
        rs <- row_sums(m > 0)
        if (any(rs == 0))
            warning("term does not occur in the corpus")
        m <- m * log2(nDocs(m) / rs)
        m$Weighting <- c(sprintf("%s%s",
                                 "term frequency - inverse document frequency",
                                 if (normalize) " (normalized)" else ""),
                         "tf-idf")
        if (isDTM) t(m) else m
    }, "term frequency - inverse document frequency", "tf-idf")

weightSMART <-
    WeightFunction(function(m, spec = "nnn", control = list()) {
        if (nchar(spec) != 3)
            stop("invalid spec")

        isDTM <- inherits(m, "DocumentTermMatrix")
        if (isDTM) m <- t(m)

        term_frequency <- substr(spec, 1, 1)
        document_frequency <- substr(spec, 2, 2)
        normalization <- substr(spec, 3, 3)

        # Term frequency
        tf <- switch(term_frequency,
                     # natural
                     n = m,
                     # logarithm
                     l = {
                         m$v <- 1 + log2(m$v)
                         m
                     },
                     # boolean
                     b = {
                         m$v <- rep(1, length(m$v))
                         m
                     })

        # Document frequency
        rs <- row_sums(m > 0)
        if (any(rs == 0))
            warning("term does not occur in the corpus")
        df <- switch(term_frequency,
                     # natural
                     n = 1,
                     # idf
                     t = log2(nDocs(m) / rs),
                     # prob idf
                     p = max(0, log2((nDocs(m) - rs) / rs)))

        # Normalization
        norm <- switch(normalization,
                       # none
                       n = rep(1, nDocs(m)),
                       # byte size
                       b = control$charlength^control$alpha)

        m <- tf * df
        names(norm) <- seq_len(nDocs(m))
        m$v <- m$v / norm[m$j]
        m$Weighting <- c(paste("SMART", spec), "SMART")

        if (isDTM) t(m) else m
    }, "SMART", "SMART")

weightBin <-
    WeightFunction(function(m) {
        m$v <- rep(1, length(m$v))
        m$Weighting <- c("binary", "bin")
        m
    }, "binary", "bin")
