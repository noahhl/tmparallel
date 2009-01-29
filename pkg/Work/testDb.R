library("tm")
data("crude")
reut21578 <- system.file("texts", "reut21578", package = "tm")
r <- Corpus(DirSource(reut21578),
                readerControl = list(reader = readReut21578XML, language = "en_US", load = FALSE),
                dbControl = list(useDb = TRUE, dbName = "~/temp/reutdb", dbType = "DB1"))
show(r)
summary(r)
inspect(r[2:3])
r <- tmMap(r, loadDoc)
r <- tmMap(r, asPlain, convertReut21578XMLPlain)
r <- tmMap(r, stripWhitespace)
r <- tmMap(r, tmTolower)
r <- tmMap(r, removeWords, stopwords("english"))
r <- tmMap(r, stemDoc)
query <- "identifier == '10' & heading == 'COMPUTER TERMINAL SYSTEMS <CPML> COMPLETES SALE'"
tmFilter(r, query)
tmIndex(r, FUN = searchFullText, "partnership", doclevel = TRUE)
r <- appendElem(r, crude[[1]], 0)
r <- appendMeta(r, cmeta = list(test = c(1,2,3)), dmeta = list(cl1 = 1:11))
summary(r)
CMetaData(r)
DMetaData(r)
r <- removeMeta(r, cname = "test", dname = "cl1")
CMetaData(r)
DMetaData(r)
crude[[1]] %IN% r
crude[[2]] %IN% r
tdm <- TermDocMatrix(r)
Data(tdm)[1:8,50:55]
