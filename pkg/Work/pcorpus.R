library("tmparallel")
acq <- system.file("texts", "acq", package = "tmparallel")
tmpfile <- tempfile()
r <- PCorpus(DirSource(acq),
             readerControl = list(reader = readReut21578XML),
             dbControl = list(dbName = tmpfile, dbType = "DB1"))
print(r)
summary(r)
r <- tm_map(r, as.PlainTextDocument)
inspect(r[2:3])
r <- tm_map(r, stripWhitespace)
r <- tm_map(r, tolower)
r <- tm_map(r, removeWords, stopwords("english"))
r <- tm_map(r, stemDocument)
query <- "id == '10' & heading == 'COMPUTER TERMINAL SYSTEMS <CPML> COMPLETES SALE'"
tm_filter(r, FUN = sFilter, query)
tm_index(r, pattern = "partner")
meta(r, tag = "test", "corpus") <- 1:3
meta(r, tag = "cl1", "indexed") <- 1:10
summary(r)
meta(r, type = "corpus")
meta(r)
meta(r, tag = "test", "corpus") <- NULL
meta(r, tag = "cl1", "indexed") <- NULL
CMetaData(r)
DMetaData(r)
tdm <- TermDocumentMatrix(r)
inspect(tdm[50:55,1:8])

rm(r)
file.remove(tmpfile)
