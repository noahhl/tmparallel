library("tm")

acqDir <- system.file("texts", "acq", package = "tm")
acqXML <- Corpus(DirSource(acqDir), readerControl = list(reader = readReut21578XML))
acq <- tmMap(acqXML, asPlain)
save(acq, file = "../data/acq.rda", compress = TRUE)

crudeDir <- system.file("texts", "crude", package = "tm")
crudeXML <- Corpus(DirSource(crudeDir), readerControl = list(reader = readReut21578XML))
crude <- tmMap(crudeXML, asPlain)
save(crude, file = "../data/crude.rda", compress = TRUE)
