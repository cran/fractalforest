#' Rewrite an Axiom Using Production Rules to Generate a String for Turtle Graphics
#'
#' @description
#' This function iterates over a set of production rules to transform an initial axiom (a string) into a final string that can be used for turtle graphics. The rules are applied to the axiom over a specified number of iterations, with each iteration updating the string according to the rules provided.
#'
#' @param init A character string representing the initial axiom (starting string) for the L-system.
#' @param rules A data frame with two columns: `inp` (input variables) and `out` (corresponding replacement strings). The rules define how each symbol in the axiom should be replaced during the iterations.
#' @param n_iter An integer specifying the number of iterations or cycles to apply the production rules. Default is 5.
#' @param verbose An integer controlling the verbosity of the output during iterations.
#'        - `verbose = 1L`: prints basic information about each cycle.
#'        - `verbose > 1L`: provides detailed information about each step.
#'        - `verbose < 1L`: suppresses output.
#'
#' @return A character string that represents the final string after all iterations, ready for use in turtle graphics.
#'
#' @details
#' The function applies the provided production rules to the initial axiom for the specified number of iterations. In each cycle, the rules are applied simultaneously to the current string, and the resulting string is updated. The function also supports different verbosity levels for reporting the transformation progress.
#'
#' @examples
#' # Example of a simple L-system with a binary tree
#' init <- "0"
#' rules <- data.frame(
#'   inp = c("0", "1"),
#'   out = c("1[-(0)]+(0)", "1"),
#'   stringsAsFactors = FALSE
#' )
#' result <- iterate_lsystem(init, rules, n_iter = 3, verbose = 1L)
#' print(result)
#'
#' @export
iterate_lsystem <- function(init = NULL, rules = NULL, n_iter = 5,
                            verbose = 0L) {

  nc <- nchar(rules$inp)
  if (any(nc > 1)) stop("Input variables must be a single character")

  if (verbose == 1L) cat("\nCycle 0 string has length ", nchar(init), "\n", sep = "")
  if (verbose == 1L) cat("Cycle 0:", init, "\n")
  curr <- init
  out <- rep(NA_character_, n_iter+1)
  out[1] <- init

  for (j in 1:n_iter) {

    RR <- vector("list")
    for (i in 1:nrow(rules)) {
      rr <- stringr::str_locate_all(curr, rules[i,1])
      if (verbose > 1L) cat("Processing rule", i, "\n")
      if (dim(rr[[1]])[1] == 0) {
        if (verbose > 1L) cat("\tRule", i, "was not needed\n")
        next
      }
      RR[i] <- rr
    }

    if (verbose >= 1L) print(RR)


    for (i in 1:length(RR)) {
      tmp <- as.data.frame(RR[i])
      if (is.null(tmp[1,1])) {

        next
      }

      rule_i <- rules[i,2]
      if(grepl(',', rule_i)){
        rules_i <- stringr::str_split(rule_i, ',', simplify = TRUE)
        rule_i <- sample(rules_i, 1)
      }

      tmp$insert <- rule_i
      RR[[i]] <- tmp
    }

    if (verbose >= 1L) print(RR)

    RRdf <- as.data.frame(RR[1])
    for (i in 2:length(RR)) {
      RRdf <- rbind(RRdf, as.data.frame(RR[i]))
    }

    if (verbose > 1L) print(RRdf)

    curr <- unlist(strsplit(curr, ""))
    curr[RRdf$start] <- RRdf$insert
    curr <- paste0(curr, collapse = "")
    out[j+1] <- curr
    if (verbose == 1L) cat("\nCycle ", j, " string has length ", nchar(curr), "\n", sep = "")
    if (verbose == 1L) cat("Cycle ", j, ": ", curr, "\n", sep = "")
  }

  curr
}
