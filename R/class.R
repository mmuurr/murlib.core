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


## Because consider x <- 1
## class(x) == "integer", but attr(x, "class") is NULL.
## We should preserve the NULLness, I think.
## The class(x) is fallback behavior when the "class" attribute is NULL.
## By assigning a class explicitly, we are bypassing this fallback.
class_attr <- function(x) attr(x, "class", TRUE)


#' @param null replace `NULL` with this val. If `NULL`, then `NULL` is returned (without a class).
#' @export
prepend_class <- function(x, cls = character(0), null = NULL) {
  if (is.null(x)) x <- null  ## x is NULL? replace with `null`.
  if (is.null(x)) return(NULL)  ## x is still NULL? return it.
  structure(x, class = unique(c(cls, class_attr(x))))
}


#' @param null replace `NULL` with this val. If `NULL`, then `NULL` is returned (without a class).
#' @export
append_class <- function(x, cls = character(0), null = NULL) {
  if (is.null(x)) x <- null  ## x is NULL? replace with `null`.
  if (is.null(x)) return(NULL)  ## x is still NULL? return it.
  structure(x, class = unique(c(class_attr(x), cls)))
}
