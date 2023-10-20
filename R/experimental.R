`%enumin%` <- (function() {
  
  trim <- function(x) {
    x <- gsub("^[[:space:]]*", "", x)
    x <- gsub("[[:space:]]*$", "", x)
    x
  }
  #trim <- memoise::memoise(trim)  ## don't do this in murlib.core, keep it dependency-free (apart from R's stdlib)
  
  
  ## returned function def:
  function(l,r) {
    l <- tolower(trim(l))
    r <- tolower(trim(r))
    l %in% r
  }
})()
