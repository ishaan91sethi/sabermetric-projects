# Omits the first character of a string if it is an "@"

omit_first_character(x) {
  len <- length(x)
  if (substr(x, 1, 1) = "@") {
    return(substr(x, 2, len))
  }
  else {
    return(x)
  }
    
}