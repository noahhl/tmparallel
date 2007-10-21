# Author: Ingo Feinerer

completeStems <- function(object, words) {
    # Get a list of all terms from the collection
    terms <- unique(unlist(lapply(object, strsplit, "[^[:alnum:]]+")))
    # As heuristic just take the first found completion
    sapply(words, function(w) grep(sprintf("^%s", w), terms, value = TRUE)[1])
}
