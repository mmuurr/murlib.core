na2 <- function(x, replacement) {
    ifelse(is.na(x), x, replacement)
}

nan2 <- function(x, replacement) {
    ifelse(is.nan(x), x, replacement)
}

inf2 <- function(x, replacement) {
    ifelse(is.infinite(x), x, replacement)
}

