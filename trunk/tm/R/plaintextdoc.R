setMethod("show",
          signature(object = "PlainTextDocument"),
          function(object){
              cat(Corpus(object), "\n")
    })
