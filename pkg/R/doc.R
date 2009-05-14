setMethod("show",
          signature(object = "PlainTextDocument"),
          function(object){
              print(noquote(Content(object)))
    })

print.MinimalDocument <- function(x, ...)
    print(noquote(as.character(x)))
