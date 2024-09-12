#' @title Not-in
#' @name not_in

#' @rdname not_in
#' @export
`%!in%` <- function(l,r) !(l %in% r)

#' @rdname not_in
#' @export
`%notin%` <- `%!in%`
