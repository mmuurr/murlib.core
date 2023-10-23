##==============================================================================
## Previous versions of the code below looked like so:
##   ifelse(is.na(x), x, replacement)  # for na2
## But ifelse (annoyingly) declasses matrices :-|.
## Hence the explicit bracketed assignment versions seen below.
## (There's a warning about this behavior in the ifelse reference page.)
##==============================================================================

#' @export
na2 <- function(x, replacement) {
  x[is.na(x)] <- replacement
  x
}

#' @export
nan2 <- function(x, replacement) {
  x[is.nan(x)] <- replacement
  x
}

#' @export
inf2 <- function(x, replacement) {
  x[is.infinite(x)] <- replacement
  x
}

