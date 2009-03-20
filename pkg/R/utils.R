## Helper functions

.xml_value_if_not_null <- function(n, default) if (!is.null(n)) XML::xmlValue(n) else default
