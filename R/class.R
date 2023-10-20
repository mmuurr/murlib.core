inherits_all <- function(x, klass = character(0)) {
  if (is.null(klass)) klass <- character(0)

  ## v1:
  ##   all(inherits(x, klass, which = TRUE) > 0)
  ## The > 0 portion isn't needed, however, leading to the implemented code below.
  ##
  ## v2:
  ##   isa(x, klass)
  ## This is surprisingly slow, given that it's a base function.
  ## The implementation below is typically 1.5X--2X faster _after_ the initial run.
  ## (The first run of the user-defined function is slow, but then the JIT/runtime cache is activated.)
  all(inherits(x, klass, which = TRUE))
}


#' @export
prepend_class <- function(x, klass = character(0), force = FALSE) {
  if (inherits_all(x, klass) && !isTRUE(force)) return(x)
  structure(x, class = unique(c(klass, class(x))))
}


#' @export
append_class <- function(x, klass = character(0), force = FALSE) {
  if (inherits_all(x, klass) && !isTRUE(force)) return(x)
  structure(x, class = unique(c(class(x), klass)))
}
