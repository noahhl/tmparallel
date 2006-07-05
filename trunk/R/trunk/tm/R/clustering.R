# Author: Ingo Feinerer

# Distance Matrix Computation for Term Document Matrices
# TODO: Optimize distance computation for sparse matrices (like term document matrices)
setGeneric("dist", function(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2) standardGeneric("dist"))
#setMethod("dist", c("termdocmatrix"),
#          function(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2) {
              # Do some special distance matrix computation
#          })
