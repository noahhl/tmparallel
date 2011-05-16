## Author: Ingo Feinerer
## Regression tests

library("tmparallel")
data("acq")

# Lazy transformations
acq <- tm_map(acq, stemDocument, lazy = TRUE)
CMetaData(acq)
acq[[1]]
acq[[17]]
CMetaData(acq)
