# Author: Ingo Feinerer

importRIS <- function(RISDir = ".", years = c(2005), senats = c(13, 14, 15, 16, 17)) {
    require(textmin)

    # Load HTML from the local directory
    RISCol <- textdoccol(RISDir, "RIS", stripWhiteSpace = TRUE, toLower = TRUE)

    # Retain only non-empty year and senat conforming documents
    incorrectIndices <- NULL
    for (i in 1:length(RISCol)) {
        year <- substring(id(RISCol[[i]]), 1, 4)
        senat <- substring(id(RISCol[[i]]), 5, 6)

        if (!(year %in% years) || !(senat %in% senats))
            incorrectIndices <- c(incorrectIndices, i)
    }
    if (!is.null(incorrectIndices))
        RISCol <- RISCol[-incorrectIndices]

    return(RISCol)
}
