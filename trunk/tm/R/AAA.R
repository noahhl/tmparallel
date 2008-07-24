.onLoad <- function(libname, pkgname) {
    # ToDo: Make cluster size variable!
    if (require("snow", quietly = TRUE) && require("Rmpi", quietly = TRUE) && is.null(snow::getMPIcluster()))
        snow::makeMPIcluster(10)
}

.Last <- function() {
    if (require("snow", quietly = TRUE) && require("Rmpi", quietly = TRUE))
        snow::stopCluster(snow::getMPIcluster())
}
