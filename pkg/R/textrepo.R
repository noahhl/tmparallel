# Author: Ingo Feinerer

TextRepository <- function(x, meta = list(created = as.POSIXlt(Sys.time(), tz = "GMT"))) {
    x <- structure(list(x), class = c("TextRepository", "list"))
    attr(x, "RepoMetaData") <- meta
    x
}

print.TextRepository <- function(x) {
    cat(sprintf(ngettext(length(x),
                         "A text repository with %d corpus\n",
                         "A text repository with %d corpora\n"),
                length(x)))
}

summary.TextRepository <- function(x) {
    print(x)
    if (length(RepoMetaData(x)) > 0) {
        cat(sprintf(ngettext(length(RepoMetaData(x)),
                             "\nThe repository metadata consists of %d tag-value pair\n",
                             "\nThe repository metadata consists of %d tag-value pairs\n"),
                    length(RepoMetaData(x))))
        cat("Available tags are:\n")
        cat(names(RepoMetaData(x)), "\n")
    }
}
