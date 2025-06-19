#' Check if character is a pop symbol
#'
#' @param x an R object to be tested.
#'
#' @return A logical vector the same length as \code{x},
#'   with \code{TRUE} for elements equal to the pop symbol \code{"]"},
#'   and \code{FALSE} otherwise.
#'
#' @export
is_pop_symbol <- function(x){ x == ']'}

