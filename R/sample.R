## base::sample has a fundamental (and dangerous) API flaw.
## The signature is function(x, size, ...)
## In cases where x is a positive integer, the values will be sampled from 1:x.
## If x is an artibtary-length integer vector and happens for some case to be length=1, this can cause bugs.
## So here we are explicit about sampling _from_ a vector.
## Likewise, the int version is named just for consistency with sample_from.

#' @export
sample_int <- sample.int

#' @export
sample_from <- function(x, size = length(x), replace = FALSE, prob = NULL) {
    x[sample_int(length(x), size, replace, prob)]
}
