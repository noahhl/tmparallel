resolveISOCode <- function(code) {
    switch(gsub("_.*", "", code),
           "de" = "german",
           "en" = "english",
           "fr" = "french")
}
