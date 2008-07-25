clActive <- local({
    # Default is to use a cluster if available
    cla <- TRUE
    function(new, ...) {
        if (!missing(new))
            cla <<- new
        else
            cla
    }
})

activateCluster <- function() clActive(TRUE)

deactivateCluster <- function() clActive(FALSE)

.onLoad <- function(libname, pkgname) {
    if (require("snow", quietly = TRUE) && require("Rmpi", quietly = TRUE) && is.null(snow::getMPIcluster()))
        snow::makeMPIcluster(Rmpi::mpi.universe.size())
}

.Last <- function() {
    if (require("snow", quietly = TRUE) && require("Rmpi", quietly = TRUE))
        snow::stopCluster(snow::getMPIcluster())
}
