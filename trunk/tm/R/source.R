# Author: Ingo Feinerer

# Source objects

setClass("Source",
         representation(LoDSupport = "logical",
                        Position = "numeric",
                        "VIRTUAL"))

# A directory with files
setClass("DirSource",
         representation(FileList = "character",
                        Load = "logical"),
         contains = c("Source"))

# A single CSV file where each line is interpreted as document
setClass("CSVSource",
         representation(URI = "ANY",
                        Content = "character"),
         contains = c("Source"))

# A single XML file consisting of several Reuters documents
# Works both for Reuters21578XML and RCV1 XML files
setClass("ReutersSource",
         representation(URI = "ANY",
                        Content = "list"),
         contains = c("Source"))

# A single XML (RDF) file containing Gmane R mailing list archive feeds
setClass("GmaneRSource",
         representation(URI = "ANY",
                        Content = "list"),
         contains = c("Source"))


# Methods for Source objects

setGeneric("DirSource", function(directory, load = FALSE, recursive = FALSE) standardGeneric("DirSource"))
setMethod("DirSource",
          signature(directory = "character"),
          function(directory, load = FALSE, recursive = FALSE) {
              d <- dir(directory, full.names = TRUE, recursive = recursive)
              isdir <- sapply(d, file.info)["isdir",]
              files <- d[isdir == FALSE]
              new("DirSource", LoDSupport = TRUE, FileList = files,
                  Position = 0, Load = load)
          })

setGeneric("CSVSource", function(object) standardGeneric("CSVSource"))
setMethod("CSVSource",
          signature(object = "character"),
          function(object) {
              object <- substitute(file(object))
              con <- eval(object)
              content <- scan(con, what = "character")
              close(con)
              new("CSVSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })
setMethod("CSVSource",
          signature(object = "ANY"),
          function(object) {
              object <- substitute(object)
              con <- eval(object)
              content <- scan(con, what = "character")
              close(con)
              new("CSVSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })

setGeneric("ReutersSource", function(object) standardGeneric("ReutersSource"))
setMethod("ReutersSource",
          signature(object = "character"),
          function(object) {
              object <- substitute(file(object))
              con <- eval(object)
              corpus <- paste(readLines(con), "\n", collapse = "")
              close(con)
              tree <- xmlTreeParse(corpus, asText = TRUE)
              content <- xmlRoot(tree)$children

              new("ReutersSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })
setMethod("ReutersSource",
          signature(object = "ANY"),
          function(object) {
              object <- substitute(object)
              con <- eval(object)
              corpus <- paste(readLines(con), "\n", collapse = "")
              close(con)
              tree <- xmlTreeParse(corpus, asText = TRUE)
              content <- xmlRoot(tree)$children

              new("ReutersSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })

setGeneric("GmaneRSource", function(object) standardGeneric("GmaneRSource"))
setMethod("GmaneRSource",
          signature(object = "character"),
          function(object) {
              object <- substitute(file(object))
              con <- eval(object)
              corpus <- paste(readLines(con), "\n", collapse = "")
              close(con)
              tree <- xmlTreeParse(corpus, asText = TRUE)
              content <- xmlRoot(tree)$children
              content <- content[names(content) == "item"]

              new("GmaneRSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })
setMethod("GmaneRSource",
          signature(object = "ANY"),
          function(object) {
              object <- substitute(object)
              con <- eval(object)
              corpus <- paste(readLines(con), "\n", collapse = "")
              close(con)
              tree <- xmlTreeParse(corpus, asText = TRUE)
              content <- xmlRoot(tree)$children
              content <- content[names(content) == "item"]

              new("GmaneRSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })

setGeneric("step_next", function(object) standardGeneric("step_next"))
setMethod("step_next",
          signature(object = "DirSource"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })
setMethod("step_next",
          signature(object = "CSVSource"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })
setMethod("step_next",
          signature(object = "ReutersSource"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })
setMethod("step_next",
          signature(object = "GmaneRSource"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })

setGeneric("get_elem", function(object) standardGeneric("get_elem"))
setMethod("get_elem",
          signature(object = "DirSource"),
          function(object) {
              filename <- object@FileList[object@Position]
              list(content = readLines(filename),
                   uri = substitute(file(filename)))
          })
setMethod("get_elem",
          signature(object = "CSVSource"),
          function(object) {
              list(content = object@Content[object@Position],
                   uri = object@URI)
          })
setMethod("get_elem",
          signature(object = "ReutersSource"),
          function(object) {
              # Construct a character representation from the XMLNode
              con <- textConnection("virtual.file", "w")
              saveXML(object@Content[[object@Position]], con)
              close(con)

              list(content = virtual.file, uri = object@URI)
          })
setMethod("get_elem",
          signature(object = "GmaneRSource"),
          function(object) {
              # Construct a character representation from the XMLNode
              con <- textConnection("virtual.file", "w")
              saveXML(object@Content[[object@Position]], con)
              close(con)

              list(content = virtual.file, uri = object@URI)
          })

setGeneric("eoi", function(object) standardGeneric("eoi"))
setMethod("eoi",
          signature(object = "DirSource"),
          function(object) {
              if (length(object@FileList) <= object@Position)
                  return(TRUE)
              else
                  return(FALSE)
          })
setMethod("eoi",
          signature(object = "CSVSource"),
          function(object) {
              if (length(object@Content) <= object@Position)
                  return(TRUE)
              else
                  return(FALSE)
          })
setMethod("eoi",
          signature(object = "ReutersSource"),
          function(object) {
              if (length(object@Content) <= object@Position)
                  return(TRUE)
              else
                  return(FALSE)
          })
setMethod("eoi",
          signature(object = "GmaneRSource"),
          function(object) {
              if (length(object@Content) <= object@Position)
                  return(TRUE)
              else
                  return(FALSE)
          })
