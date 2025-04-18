#' @title Predicate-based object replacement
#' @details Meant to be pipe-friendly and similar in spirit to `tidyr::replace_na`, only:
#'   (i) this function is scalar, operating on the entire object as a single unit and
#'   (ii) one passes an arbitrary predicate function `test` that takes a single argument (`x`).
#' @export
default_ifnot <- function(x, test = \(x) TRUE, default = x) {
  if (isTRUE(test(x))) x else default
}


## alias
#' @rdname default_ifnot
#' @export
default_if_not <- default_ifnot


