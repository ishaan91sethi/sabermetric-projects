# Omits the first character of a string if it is an "@"

omit_at <- function(x) {
  len <- nchar(x)
  if (substr(x, 1, 1) == "@") {
    return(substr(x, 2, len))
  }
  else {
    return(x)
  }
    
}