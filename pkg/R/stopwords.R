# Map ISO 639-2 codes to languages used by the Snowball stemmer
# http://en.wikipedia.org/wiki/ISO_639-2
map_ISO_639_2 <- function(code) {
    switch(gsub("_.*", "", code),
           "dan" = "danish",
           "nld" = "dutch",
           "dut" = "dutch",
           "eng" = "english",
           "fin" = "finnish",
           "fra" = "french",
           "fre" = "french",
           "deu" = "german",
           "ger" = "german",
           "hun" = "hungarian",
           "ita" = "italian",
           "nor" = "norwegian",
           "por" = "portuguese",
           "rus" = "russian",
           "spa" = "spanish",
           "swe" = "swedish")
}

stopwords <- {
    function(language = "eng") {
        resolved <- tm:::map_ISO_639_2(language)
        lang <- if (is.null(resolved))
            language
        else
            resolved
        readLines(system.file("stopwords", paste(lang, ".dat", sep = ""), package = "tm"), encoding = "UTF-8")
    }
}
