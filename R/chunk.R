#' @title Chunk a vector into a list of vectors of roughly equal length.
#'
#' @param x The vector or list to be chunked.
#' @param n The number of integers to chunk (i.e. chunk the sequence \code{1:n}).
#' @param count The number of chunks desired.
#' @param max_length The maximum desired length of any chunk.
#' @param method The algorithm to use for chunking.
#'        \code{"seq"} _seq_uentially chunks \code{x}.
#'        \code{"mod"} creates chunks where original indices are alike after modulo arithmetic.
#'        \code{"rand"} specifies that \code{x} should be uniformly and randomly distributed across chunks.
#' @return For \code{chunk}, a list with \code{x} chunked across the list entries.
#'         For \code{chunk_int}, a list with the values \code{1:n} chunked across the list entries.
#' @details Exactly one of \code{n_chunks} and \code{max_chunk_size} should be \code{NULL} (to specify both as non-\code{NULL} is an error).
#' @export
chunk <- function(x, chunk_count = NULL, chunk_max_length = NULL, method = c("seq", "mod", "rand")) {
  both_null <- (is.null(chunk_count) && is.null(chunk_max_length))
  both_non_null <- (!is.null(chunk_count)) && (!is.null(chunk_max_length))
  if (both_null || both_non_null) {
    stop("exactly one of `chunk_count` or `chunk_max_length` must be non-NULL")
  }

  method <- match.arg(method)

  if (is.null(chunk_count)) {
    chunk_count <- ceiling(length(x) / chunk_max_length)
  }

  split_iix <- (seq_along(x) - 1) %% chunk_count

  ## split_iix is currently chunked by mod.  
  ## do we need to alter the method to either seq or rand?
  split_iix <-
    switch(
      method,
      "seq" = sort(split_iix),
      "mod" = split_iix,
      "rand" = sample_from(split_iix)
    )

  base::split(x, split_iix)
}


#' @rdname chunk
#' @export
chunk_int <- function(n, chunk_count = NULL, chunk_max_length = NULL, method = c("seq", "mod", "rand")) {
    chunk(seq_len(n), chunk_count, chunk_max_length, method)
}
