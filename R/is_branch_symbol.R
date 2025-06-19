#' Check if character is a branch symbol
#'
#' @param x an R object to be tested.
#'
#' @return A logical vector the same length as \code{x},
#'   with \code{TRUE} for elements that are *not* one of the branch symbols
#'   (\code{"["}, \code{"]"}, \code{"+"}, \code{"-"}, \code{"("}, \code{")"}, \code{"<"}, \code{">"}),
#'   and \code{FALSE} otherwise.
#'
#' @export
is_branch_symbol <- function(x) {!x %in% c('[',']','+','-','(',')','<','>')}
