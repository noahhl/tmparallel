.onLoad <- function(libname, pkgname) {
    # ToDo: Make cluster size variable!
    if (require("snow", quietly = TRUE) && require("Rmpi", quietly = TRUE) && is.null(snow::getMPIcluster()))
        makeMPIcluster(10)
}

.Last <- function() {
    if (require("snow", quietly = TRUE) && require("Rmpi", quietly = TRUE))
        stopCluster(snow::getMPIcluster())
}
