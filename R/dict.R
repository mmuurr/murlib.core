foo <- (function() {

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
		if (!is.list(x)) return(FALSE)
		if (!is_named(x)) return(FALSE)
		if (any(duplicated(names(x)))) return(FALSE)
		TRUE
  }

	as_dict <- function(x) {
		if (length(x) == 0) return(dict())
		x <- as.list(x)
		if (!is_dict(x)) stop("cannot coerce to dict")
		x
	}

  dict <- function(...) {
    if (...length() == 0) return(structure(list(), names = character(0)))
    x <- list(...)
    if (!isTRUE(is_dict(x))) stop("not dictionaryish")
    x
  }

  list(dict = dict, is_dict = is_dict, as_dict = as_dict)
})()

#' @name dictionary
#' @title Dictionary-list

#' @rdname dictionary
#' @export
dict <- foo$dict

#' @rdname dictionary
#' @export
is_dict <- foo$is_dict

#' @rdname dictionary
#' @export
as_dict <- foo$as_dict


rm(foo)
