# Author: Ingo Feinerer

setGeneric("textdoccol", function(object, inputType = "CSV", stripWhiteSpace = FALSE, toLower = FALSE) standardGeneric("textdoccol"))
setMethod("textdoccol",
          c("character"),
          function(object, inputType = "CSV", stripWhiteSpace = FALSE, toLower = FALSE) {
              # Add a new type for each unique input source format
              type <- match.arg(inputType,c("CSV", "RCV1", "REUT21578", "RIS"))
              switch(type,
                     # Text in a special CSV format
                     # For details on the file format see the R documentation file
                     # The first argument is a directory with .csv files
                     "CSV" = {
                         filelist <- dir(object, pattern = ".csv", full.names = TRUE)
                         tdl <- sapply(filelist,
                                       function(file) {
                                           m <- as.matrix(read.csv(file, header = FALSE))
                                           l <- vector("list", dim(m)[1])
                                           for (i in 1:dim(m)[1]) {
                                               author <- ""
                                               datetimestamp <- date()
                                               description <- ""
                                               id <- as.integer(m[i,1])
                                               corpus <- as.character(m[i,2:dim(m)[2]])
                                               if (stripWhiteSpace)
                                                   corpus <- gsub("[[:space:]]+", " ", corpus)
                                               if (toLower)
                                                   corpus <- tolower(corpus)
                                               origin <- "CSV"
                                               heading <- ""

                                               l[[i]] <- new("textdocument", .Data = corpus, author = author, datetimestamp = datetimestamp,
                                                             description = description, id = id, origin = origin, heading = heading)
                                           }
                                           l
                                       })
                         if (length(filelist) > 1)
                             tdcl <- new("textdoccol", .Data = unlist(tdl, recursive = FALSE))
                         else
                             tdcl <- new("textdoccol", .Data = tdl)
                     },
                     # Read in text documents in XML Reuters Corpus Volume 1 (RCV1) format
                     # The first argument is a directory with the RCV1 XML files
                     "RCV1" = {
                         filelist <- dir(object, pattern = ".xml", full.names = TRUE)
                         tdl <- sapply(filelist,
                                       function(file) {
                                           tree <- xmlTreeParse(file)
                                           xmlApply(xmlRoot(tree), parseNewsItem, stripWhiteSpace, toLower)
                                       })
                         if (length(filelist) > 1)
                             tdcl <- new("textdoccol", .Data = unlist(tdl, recursive = FALSE))
                         else
                             tdcl <- new("textdoccol", .Data = tdl)
                     },
                     # Read in text documents in Reuters-21578 XML (not SGML) format
                     # Typically the first argument will be a directory where we can
                     # find the files reut2-000.xml ... reut2-021.xml
                     "REUT21578" = {
                         filelist <- dir(object, pattern = ".xml", full.names = TRUE)
                         tdl <- sapply(filelist,
                                       function(file) {
                                           tree <- xmlTreeParse(file)
                                           xmlApply(xmlRoot(tree), parseReuters, stripWhiteSpace, toLower)
                                       })
                         if (length(filelist) > 1)
                             tdcl <- new("textdoccol", .Data = unlist(tdl, recursive = FALSE))
                         else
                             tdcl <- new("textdoccol", .Data = tdl)
                     },
                     # Read in HTML documents as used by http://ris.bka.gv.at/vwgh
                     "RIS" = {
                         filelist <- dir(object, pattern = ".html", full.names = TRUE)
                         tdl <- sapply(filelist,
                                       function(file) {
                                           # Ignore warnings from misformed HTML documents
                                           suppressWarnings(RISDoc <- parseHTML(file, stripWhiteSpace, toLower))
                                           if (!is.null(RISDoc)) {
                                               l <- list()
                                               l[[length(l) + 1]] <- RISDoc
                                               l
                                           }
                                       })
                         tdcl <- new("textdoccol", .Data = tdl)
                     })
              tdcl
          })

# Parse an Austrian RIS HTML document
parseHTML <- function(file, stripWhiteSpace = FALSE, toLower = FALSE) {
    author <- ""
    datetimestamp <- date()
    description <- ""

    tree <- htmlTreeParse(file)
    htmlElem <- unlist(tree$children$html$children)

    if (is.null(htmlElem))
        stop(paste("Empty document", file, "cannot be processed."))

    textElem <- htmlElem[which(regexpr("text.value", names(htmlElem)) > 0)]
    names(textElem) <- NULL

    corpus <- paste(textElem, collapse = " ")

    year <- substring(corpus, regexpr("..../../", corpus), regexpr("..../../", corpus) + 3)
    senat <- substring(corpus, regexpr("..../../", corpus) + 5, regexpr("..../../", corpus) + 6)
    number <- substring(corpus, regexpr("..../../", corpus) + 8, regexpr("..../../", corpus) + 11)

    id <- as.integer(paste(year, senat, number, sep = ""))

    if (is.na(id))
        stop(paste("Cannot extract 'Geschaeftszahl' out of malformed document", file))
    origin <- ""

    if (stripWhiteSpace)
        corpus <- gsub("[[:space:]]+", " ", corpus)
    if (toLower)
        corpus <- tolower(corpus)

    heading <- ""

    new("textdocument", .Data = corpus, author = author, datetimestamp = datetimestamp,
        description = description, id = id, origin = origin, heading = heading)
}

# TODO: Implement lacking fields as soon I have access to the original RCV1
# Parse a <newsitem></newsitem> element from a well-formed RCV1 XML file
parseNewsItem <- function(node, stripWhiteSpace = FALSE, toLower = FALSE) {
    author <- "Not yet implemented"
    datetimestamp <- xmlAttrs(node)[["date"]]
    description <- "Not yet implemented"
    id <- as.integer(xmlAttrs(node)[["itemid"]])
    origin <- "Reuters Corpus Volume 1 XML"
    corpus <- unlist(xmlApply(node[["text"]], xmlValue), use.names = FALSE)

    if (stripWhiteSpace)
        corpus <- gsub("[[:space:]]+", " ", corpus)
    if (toLower)
        corpus <- tolower(corpus)

    heading <- xmlValue(node[["title"]])

    new("textdocument", .Data = corpus, author = author, datetimestamp = datetimestamp,
        description = description, id = id, origin = origin, heading = heading)
}

# Parse a <REUTERS></REUTERS> element from a well-formed Reuters-21578 XML file
parseReuters <- function(node, stripWhiteSpace = FALSE, toLower = FALSE) {
    # The <AUTHOR></AUTHOR> tag is unfortunately NOT obligatory!
    if (!is.null(node[["TEXT"]][["AUTHOR"]]))
        author <- xmlValue(node[["TEXT"]][["AUTHOR"]])
    else
        author <- ""

    datetimestamp <- xmlValue(node[["DATE"]])
    description <- ""
    id <- as.integer(xmlAttrs(node)[["NEWID"]])

    origin <- "Reuters-21578 XML"

    # The <BODY></BODY> tag is unfortunately NOT obligatory!
    if (!is.null(node[["TEXT"]][["BODY"]]))
        corpus <- xmlValue(node[["TEXT"]][["BODY"]])
    else
        corpus <- ""

    if (stripWhiteSpace)
        corpus <- gsub("[[:space:]]+", " ", corpus)
    if (toLower)
        corpus <- tolower(corpus)

    # The <TITLE></TITLE> tag is unfortunately NOT obligatory!
    if (!is.null(node[["TEXT"]][["TITLE"]]))
        heading <- xmlValue(node[["TEXT"]][["TITLE"]])
    else
        heading <- ""

    new("textdocument", .Data = corpus, author = author, datetimestamp = datetimestamp,
        description = description, id = id, origin = origin, heading = heading)
}
