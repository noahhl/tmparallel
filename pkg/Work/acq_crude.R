library("tmparallel")

acqDir <- system.file("texts", "acq", package = "tmparallel")
acq <- Corpus(DirSource(acqDir), readerControl = list(reader = readReut21578XMLasPlain))
save(acq, file = "../data/acq.rda", compress = TRUE)

crudeDir <- system.file("texts", "crude", package = "tmparallel")
crude <- Corpus(DirSource(crudeDir), readerControl = list(reader = readReut21578XMLasPlain))
save(crude, file = "../data/crude.rda", compress = TRUE)
