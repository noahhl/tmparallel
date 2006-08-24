# Author: Ingo Feinerer

setGeneric("dissimilarity", function(x, y, method) standardGeneric("dissimilarity"))
setMethod("dissimilarity",
          signature(x = "TermDocMatrix", y = "ANY", method = "character"),
          function(x, y = NULL, method = "cosine") {
              type <- match.arg(method, c("cosine","pearson","extjacc"))

              m <- matrix(0, nrow = nrow(x), ncol = nrow(x))
              for (i in 1:nrow(m)) {
                  for (j in 1:ncol(m)) {
                      # Do not compute symmetric values
                      if (0 == m[j,i])
                          switch(type,
                                 "cosine" = {
                                     m[i,j] <- cosinus(x[i,],x[j,])
                                 },
                                 "pearson" = {
                                     m[i,j] <- pearson(x[i,],x[j,])
                                 },
                                 "extjacc" = {
                                     m[i,j] <- extjacc(x[i,],x[j,])
                                 }
                                 )
                      else
                          m[i,j] <- m[j,i]
                  }
              }
              d.vec <- as.vector(m)
              d <- numeric(0)
              a <- k <- 1
              while (k <= length(d.vec)) {
                  if (k %% nrow(x) == 1) {
                      k <- k + a
                      a <- a + 1
                  }
                  if (k <= length(d.vec))
                      d <- c(d, d.vec[k])
                  k <- k + 1
              }
              attr(d, "Size") <- nrow(x)
              attr(d, "Labels") <- dimnames(x)[[1]]
              attr(d, "Diag") <- FALSE
              attr(d, "Upper") <- FALSE
              switch(type,
                     "cosine" = {
                         attr(d, "method") <- "cosine"
                     },
                     "pearson" = {
                         attr(d, "method") <- "pearson"
                     },
                     "extjacc" = {
                         attr(d, "method") <- "extjacc"
                     }
                     )
              class(d) <- "dist"
              return(1 - d)
          })
setMethod("dissimilarity",
          c("TextDocument", "TextDocument", "character"),
          function(x, y, method) {
              tdm <- TermDocMatrix(as(list(x,y), "TextDocCol"))
              dissim <- dissimilarity(tdm, method = method)
              class(dissim) <- "dist"
              return(dissim)
          })

cosinus <- function(x, y) {
    if (!(is.vector(x) && is.vector(y)))
        stop("Invalid input")
    crossprod(x, y) / sqrt(crossprod(x) * crossprod(y))
}

pearson <- function(x, y) {
    if (!(is.vector(x) && is.vector(y)))
        stop("Invalid input")

    xa <- (x - mean(x))
    xb <- (y - mean(y))

    1/2 * ((crossprod(xa, xb) / sqrt(crossprod(xa) * crossprod(xb))) + 1)
}

extjacc <- function(x, y) {
    if (!(is.vector(x) && is.vector(y)))
        stop("Invalid input")
    crossprod(x, y) / (crossprod(x) + crossprod(y) - crossprod(x, y))
}
