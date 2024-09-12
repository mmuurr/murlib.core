#' @title Replace `NULL`
#' @name replace_null

#' @rdname replace_null
#' @export
null2 <- function(x, replacement) {
  if (is.null(x)) replacement else x
}

#' @rdname replace_null
#' @export
null_to <- null2

