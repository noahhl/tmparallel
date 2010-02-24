## This script downloads TAGS from the general inquirer web site

library("XML")
baseurl <- "http://www.webuse.umd.edu:9090/tags"

general_inquirer <- list()
src_list <- c( fall = "TAGFall.html",
               negative = "TAGNeg.html",
               positive = "TAGPos.html",
               rise = "TAGRise.html" )

get_general_inquierer_tags <- function(base_url, src){
    con <- url( file.path(baseurl, src) )
    lines <- readLines(con)
    close(con)
    html <- htmlTreeParse(lines)
    ul <- unlist(html)
    gr <- grep("a.children.text.value", names(ul))
    unique( sub("#.*", "", tolower(ul[gr])) )
}
for( i in seq_along(src_list) ){
    general_inquirer[[i]] <- get_general_inquierer_tags( baseurl, src_list[i] )
}

names(general_inquirer) <- names(src_list)

##save(general_inquirer, file = "../data/general_inquirer.Rda", compress = TRUE)
