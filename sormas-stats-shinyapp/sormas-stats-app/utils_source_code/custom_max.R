# A custom function to prevent warning messages when the vector to compute min has length zero
custom_max <- function(x) {if (length(x)>0) max(x) else Inf}