# A custom function to prevent warning messages when the vector to compute min has length zero
custom_min <- function(x) {if (length(x)>0) min(x) else Inf}