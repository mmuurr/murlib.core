`%enumin%` <- (function() {

    trim <- function(x) {
        x <- gsub("^[[:space:]]*", "", x)
        x <- gsub("[[:space:]]*$", "", x)
        x
    }

    ## returned function def:
    function(l,r) {
        l <- tolower(trim(l))
        r <- tolower(trim(r))
        l %in% r
    }
})()
