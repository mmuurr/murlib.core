#' @export
na2 <- function(x, replacement) {
    ifelse(is.na(x), x, replacement)
}

#' @export
nan2 <- function(x, replacement) {
    ifelse(is.nan(x), x, replacement)
}

#' @export
inf2 <- function(x, replacement) {
    ifelse(is.infinite(x), x, replacement)
}

