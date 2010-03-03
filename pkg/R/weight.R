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

weightBin <-
    WeightFunction(function(m) {
        m$v <- rep(1, length(m$v))
        m$Weighting <- c("binary", "bin")
        m
    }, "binary", "bin")
