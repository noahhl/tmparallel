clusterAvailable <- function() {
    require("snow", quietly = TRUE) && require("Rmpi", quietly = TRUE) && !is.null(snow::getMPIcluster())
}
