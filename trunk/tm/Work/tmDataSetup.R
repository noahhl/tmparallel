library("tm")

acqDir <- system.file("texts", "acq", package = "tm")
acqXML <- TextDocCol(DirSource(acqDir), parser = readReut21578XML, load = TRUE)
acq <- tmMap(acqXML, asPlain, convertReut21578XMLPlain)
save(acq, file = "../data/acq.rda", compress = TRUE)

crudeDir <- system.file("texts", "crude", package = "tm")
crudeXML <- TextDocCol(DirSource(crudeDir), parser = readReut21578XML, load = TRUE)
crude <- tmMap(crudeXML, asPlain, convertReut21578XMLPlain)
save(crude, file = "../data/crude.rda", compress = TRUE)
