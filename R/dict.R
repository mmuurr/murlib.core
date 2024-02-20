#' @export
dict <- (function() {

  ## rlang:::detect_void_name
  detect_void_name <- function(x) {
    x == "" | is.na(x)
  }

  ## rlang::is_null
  is_null <- function(x) {
    is.null(x)
  }

  ## rlang::is_named
  is_named <- function(x) {
    nms <- names(x)
    if (is_null(nms)) {
        return(FALSE)
    }
    if (any(detect_void_name(nms))) {
        return(FALSE)
    }
    TRUE
  }

  ## rlang::is_dictionaryish
  is_dict <- function(x) {
    if (!length(x)) {
        return(!is.null(x))
    }
    is_named(x) && !any(duplicated(names(x)))
  }

  function(...) {
    x <- list(...)
    if (!isTRUE(is_dict(x))) stop("not dictionaryish")
    x
  }
})()
