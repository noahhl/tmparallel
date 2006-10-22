# Author: Ingo Feinerer

# Preprocess the Reuters21578 XML data
# from https://www.cs.tcd.ie/courses/baict/baim/ss/part2/sw/reuters21578-xml.tar.bz2
# for further easy handling in R
preprocessReuters21578XML <- function(reuters.dir, reuters.oapf.dir) {
    dir.create(reuters.oapf.dir, recursive = TRUE)
    files <- dir(reuters.dir, pattern = "\\.xml", full.names = TRUE)

    # Correct invalid UTF-8 encoding
    # The invalid multibyte string is in reut2-017.xml at line 35578
    # from https://www.cs.tcd.ie/courses/baict/baim/ss/part2/sw/reuters21578-xml.tar.bz2

    bad.file.index <- grep("reut2-017\\.xml", files)
    content <- readLines(paste(reuters.dir, "reut2-017.xml", sep = ""))
    content[35578] <- "world economic growth. side measures to boost growth, he said."
    writeLines(content, paste(reuters.dir, "reut2-017.xml", sep = ""))

    # Write out each article in a seperate file

    counter <- 1
    for (f in files) {
        tree <- xmlTreeParse(f)
        xmlApply(xmlRoot(tree),
                 function(article) {
                     output.file <- paste(reuters.oapf.dir, "reut-",
                                          gsub(" ", "0", format(counter, width = 5)),
                                          ".xml", sep = "")
                     counter <<- counter + 1
                     saveXML(article, file = output.file)
                 })
    }
}
