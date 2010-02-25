tm_tag_score <- function(x, tags, fun, ...) UseMethod("tm_tag_score", x)
tm_tag_score.PlainTextDocument <-
function(x, tags, fun = function(x) sum(x, na.rm = TRUE), control = list(removePunctuation = TRUE))
    tm_tag_score(termFreq(x, control), tags, fun)
tm_tag_score.numeric <- function(x, tags, fun)
    fun(x[match(tags, names(x))])
