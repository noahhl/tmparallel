## Author: Ingo Feinerer
## Regression tests

library("tm")

rdf <- system.file("texts", "gmane.comp.lang.r.gr.rdf", package = "tm")
reut21578 <- system.file("texts", "reut21578", package = "tm")
rcv1 <- system.file("texts", "rcv1", package = "tm")

g <- Corpus(GmaneSource(rdf), readerControl = list(reader = readGmane))
re <- Corpus(DirSource(reut21578), readerControl = list(reader = readReut21578XML))
rc <- Corpus(DirSource(rcv1), readerControl = list(reader = readRCV1))
