library("tm")

acqDir <- system.file("texts", "acq", package = "tm")
acqXML <- Corpus(DirSource(acqDir), readerControl = list(reader = readReut21578XML, language = "en_US", load = TRUE))
acq <- tmMap(acqXML, asPlain)
save(acq, file = "../data/acq.rda", compress = TRUE)

crudeDir <- system.file("texts", "crude", package = "tm")
crudeXML <- Corpus(DirSource(crudeDir), readerControl = list(reader = readReut21578XML, language = "en_US", load = TRUE))
crude <- tmMap(crudeXML, asPlain)
save(crude, file = "../data/crude.rda", compress = TRUE)
