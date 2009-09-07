library("tm")

acqDir <- system.file("texts", "acq", package = "tm")
acq <- Corpus(DirSource(acqDir), readerControl = list(reader = readReut21578XMLasPlain))
save(acq, file = "../data/acq.rda", compress = TRUE)

crudeDir <- system.file("texts", "crude", package = "tm")
crude <- Corpus(DirSource(crudeDir), readerControl = list(reader = readReut21578XMLasPlain))
save(crude, file = "../data/crude.rda", compress = TRUE)
