resolveISOCode <- function(code) {
    code <- gsub("_.*", "", code)
    isocode <- match.arg(code, c("de", "en", "fr"))
    switch(isocode,
           "de" = "german",
           "en" = "english",
           "fr" = "french")
}
