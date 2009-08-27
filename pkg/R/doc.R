.TextDocument <-
    function(x, author, datetimestamp, description, heading, id, origin, language, localmetadata)
{
    doc <- x
    attr(doc, "Author") <- author
    attr(doc, "DateTimeStamp") <- datetimestamp
    attr(doc, "Description") <- description
    attr(doc, "Heading") <- heading
    attr(doc, "ID") <- id
    attr(doc, "Language") <- language
    attr(doc, "LocalMetaData") <- localmetadata
    attr(doc, "Origin") <- origin
    doc
}

PlainTextDocument <-
    function(x = character(0), author = character(0), datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
             description = character(0), heading = character(0), id = character(0), origin = character(0),
             language = character(0), localmetadata = list())
{
    doc <- .TextDocument(as.character(x), author, datetimestamp, description, heading, id, origin, language, localmetadata)
    class(doc) <- c("PlainTextDocument", "TextDocument", "character")
    doc
}

print.PlainTextDocument <- function(x, ...) {
    cat(noquote(as.character(x)), sep = "\n")
    invisible(x)
}

RCV1Document <-
    function(x = list(), author = character(0), datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
             description = character(0), heading = character(0), id = character(0), origin = character(0),
             language = character(0), localmetadata = list())
{
    doc <- .TextDocument(x, author, datetimestamp, description, heading, id, origin, language, localmetadata)
    class(doc) <- c("RCV1Document", "TextDocument", "XMLDocument", "XMLAbstractDocument", "oldClass")
    doc
}

Reuters21578Document <-
    function(x = list(), author = character(0), datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
             description = character(0), heading = character(0), id = character(0), origin = character(0),
             language = character(0), localmetadata = list())
{
    doc <- .TextDocument(x, author, datetimestamp, description, heading, id, origin, language, localmetadata)
    class(doc) <- c("Reuters21578Document", "TextDocument", "XMLDocument", "XMLAbstractDocument", "oldClass")
    doc
}
