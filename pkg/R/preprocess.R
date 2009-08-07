# Author: Ingo Feinerer

# Preprocess the Reuters21578 XML data
preprocessReut21578XML <- function(ReutersDir, ReutersOapfDir, fixEnc = TRUE) {
    require("XML")

    dir.create(ReutersOapfDir, recursive = TRUE)
    files <- dir(ReutersDir, pattern = "\\.xml", full.names = TRUE)

    if (fixEnc) {
        # Correct invalid UTF-8 encoding
        # The invalid multibyte string is in reut2-017.xml at line 35578
        content <- readLines(paste(ReutersDir, "reut2-017.xml", sep = ""))
        content[35578] <- "world economic growth. side measures to boost growth, he said."
        writeLines(content, paste(ReutersDir, "reut2-017.xml", sep = ""))
    }

    # Write out each article in a seperate file
    counter <- 1
    for (f in files) {
        tree <- XML::xmlTreeParse(f)
        XML::xmlApply(XML::xmlRoot(tree),
                 function(article) {
                     output.file <- paste(ReutersOapfDir, "reut-",
                                          gsub(" ", "0", format(counter, width = 5)),
                                          ".xml", sep = "")
                     counter <<- counter + 1
                     con <- file(output.file, "w")
                     XML::saveXML(article, file = con)
                     close(con)
                 })
    }
}
