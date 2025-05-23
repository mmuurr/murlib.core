##==============================================================================
## Previous versions of the code below looked like so:
##   ifelse(is.na(x), x, replacement)  # for na2
## But ifelse (annoyingly) declasses matrices :-|.
## Hence the explicit bracketed assignment versions seen below.
## (There's a warning about this behavior in the ifelse reference page.)
##==============================================================================

#' @title Replace specials (NA, NaN, (+/-)Inf)
#' @name replace_na

#' @rdname replace_na
#' @export
na2 <- function(x, replacement) {
  x[is.na(x)] <- replacement
  x
}

#' @rdname replace_na
#' @export
nan2 <- function(x, replacement) {
  x[is.nan(x)] <- replacement
  x
}

#' @rdname replace_na
#' @export
inf2 <- function(x, replacement) {
  x[is.infinite(x)] <- replacement
  x
}

#' @rdname replace_na
#' @export
na_to <- na2

#' @rdname replace_na
#' @export
nan_to <- nan2

#' @rdname replace_na
#' @export
inf_to <- inf2
