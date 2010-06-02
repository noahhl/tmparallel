plot.TermDocumentMatrix <- plot.DocumentTermMatrix <-
function(x,
         terms = sample(Terms(x), 20),
         corThreshold = 0.7,
         weighting = FALSE,
         attrs = list(graph = list(rankdir = "BT"),
         node = list(shape = "rectangle", fixedsize = FALSE)),
         ...)
{
    if (!require("Rgraphviz"))
        stop("could not find (bioconductor.org) Rgraphviz package")

    m <- if (inherits(x, "TermDocumentMatrix")) t(x) else x
    m <- as.matrix(m[, terms])
    c <- cor(m)
    c[c < corThreshold] <- 0
    diag(c) <- 0
    g <- as(c, "graphNEL")
    p <- plot(g, attrs = attrs, ...)
    if (weighting) {
        i <- 1
        lw <- round(c[lower.tri(c) & c >= corThreshold] * 10)
        for (ae in Rgraphviz::AgEdge(p)) {
            lines(ae, lwd = lw[i], len = 1)
            i <- i + 1
        }
    }
    invisible(p)
}

## Plotting functions for Zipf's and Heaps'law contributed by Kurt Hornik

## See http://en.wikipedia.org/wiki/Zipf%27s_law
Zipf_plot <-
function(m, type = "l", ...)
{
    y <- log(sort(slam::col_sums(m), decreasing = TRUE))
    x <- log(seq_along(y))
    m <- lm(y ~ x)
    plot(x, y, xlab = "log(rank)", ylab = "log(frequency)", type = type, ...)
    abline(m)
    coef(m)
}
## http://en.wikipedia.org/wiki/Heaps%27_law
## http://en.wikipedia.org/wiki/Text_corpus
## cum_vocabulary_size <-
## function(m)
## {
##     ## Should work in general, but it very slow for large simple triplet
##     ## matrices ...
##     s <- double(nrow(m))
##     v <- double(ncol(m))
##     for(i in seq_along(s)) {
##         v <- pmax(v, c(m[i, ]))
##         s[i] <- sum(v > 0)
##     }
##     s
## }
cum_vocabulary_size <-
function(m)
{
    ## Only works for simple triplet matrices.
    i <- sapply(split(m$i, m$j), min)
    tab <- table(i)
    v <- double(nrow(m))
    v[as.numeric(names(tab))] <- tab
    cumsum(v)
}

Heaps_plot <-
function(m, type = "l", ...)
{
    x <- log(cumsum(slam::row_sums(m)))
    y <- log(cum_vocabulary_size(m))
    m <- lm(y ~ x)
    plot(x, y, xlab = "log(T)", ylab = "log(V)", type = type, ...)
    abline(m)
    coef(m)
}
