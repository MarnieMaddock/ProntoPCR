#' A utility operator for handling NULL values
#'
#' This operator returns the left-hand side if it's not `NULL`, otherwise it returns the right-hand side.
#' @param a The value to check.
#' @param b The value to return if `a` is `NULL`.
#' @return `a` if it is not `NULL`; otherwise `b`.
#' @examples
#' a <- NULL
#' b <- 5
#' a %||% b  # Returns 5
#'
#' a <- 10
#' a %||% b  # Returns 10
#' @export
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

