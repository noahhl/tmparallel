setMethod("show",
          signature(object = "PlainTextDocument"),
          function(object){
              cat(noquote(Content(object)), "\n")
    })

print.MinimalDocument <- function(x, ...)
    cat(noquote(as.character(x)), "\n")
