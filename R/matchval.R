#' @export
matchval <- function(query, from, to = seq_along(from), nomatch = NA, ...) {
    match_iix <- base::match(query, from, nomatch = NA_integer_)
    na2(to[match_iix], nomatch)
}
