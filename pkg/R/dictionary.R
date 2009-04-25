# Author: Ingo Feinerer

setClass("Dictionary",
         contains = "character")

Dictionary <- function(x) UseMethod("Dictionary", x)
Dictionary.character <- function(x)
    new("Dictionary", .Data = x)
Dictionary.TermDocumentMatrix <- function(x)
    new("Dictionary", .Data = Terms(x))
