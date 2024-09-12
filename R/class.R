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


#' @title Add (S3) classes to an object
#' @name add_class
#' @details The new class vector will be uniquified, in left-to-right preferential order (so `x,y,x,z` becomes `x,y,z`).
#' @param x The object whose class is to be updated.
#' @param cls The class(es) to add.
#' @param null If `x` is `NULL`, first replace it with this value, since `NULL` can't have a class.
#' @return The object with new class structure.

#' @rdname add_class
#' @export
prepend_class <- function(x, cls = character(0), null = list()) {
  x <- null_to(x, null)
  structure(x, class = unique(c(cls, class_attr(x))))
}

#' @rdname add_class
#' @export
append_class <- function(x, cls = character(0), null = list()) {
  x <- null_to(x, null)
  structure(x, class = unique(c(class_attr(x), cls)))
}
