#' Build a 2D L-System Tree from Predefined Tree Templates
#'
#' This function generates 2D L-system trees using predefined templates. Each template is based on a specific tree model, and the function allows you to control the number of iterations to generate the desired tree structure.
#'
#' @param tree_model An integer or a character string specifying the desired tree model. The following models are available:
#' - 1 or "binary_tree" (default `n_iter` = 6)
#' - 2 or "alternate_tree" (default `n_iter` = 5)
#' - 3 or "arrow_weed" (default `n_iter` = 5)
#' - 4 or "twiggy_weed" (default `n_iter` = 5)
#' - 5 or "stochastic_fuzzy_weed" (default `n_iter` = 4)
#' - 6 or "crooked_binary_tree" (default `n_iter` = 6)
#' - 7 or "crooked_alternate_tree" (default `n_iter` = 5)
#' - 8 or "crooked_arrow_weed" (default `n_iter` = 5)
#' - 9 or "crooked_twiggy_weed" (default `n_iter` = 5)
#' - 10 or "crooked_stochastic_fuzzy_weed" (default `n_iter` = 4)
#'
#' @param n_iter An integer specifying the number of iterations to build the tree. Each tree model has its own default value, but it can be overridden by specifying this parameter.
#'
#' @details This function uses a set of predefined L-system rules to generate the tree structure. The rules for each tree model are stored in the `rules` data frame. The default number of iterations (`n_iter`) for each tree model is specified in the list above. If `n_iter` is not provided, the default value for the selected tree model will be used.
#'
#' @return A list representing the L-system tree structure after applying the specified number of iterations.
#'
#' @export
fractal_tree_model <- function(tree_model, n_iter){

  if(tree_model %in% list(1,"binary_tree")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[-(0)]+(0)", "1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 6}

  }

  else if(tree_model %in% list(2,"alternate_tree")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[-(0)]1[+(0)](0),1[+(0)]1[-(0)](0)", "1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 5}

  }

  else if(tree_model %in% list(3,"arrow_weed")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[+(0)][-(0)](10)", "1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 5}

  }

  else if(tree_model %in% list(4,"twiggy_weed")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[-(0)]1[-(0)]+(0)", "1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 5}

  }

  else if(tree_model %in% list(5,"stochastic_fuzzy_weed")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1-[[(0)]+(0)]+1[+(10)]-(0),1+[[(0)]-(0)]-1[-(10)]+(0)","1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 4}

  }

  if(tree_model %in% list(6,"crooked_binary_tree")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[-(0)]+(0)", "+-1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 6}

  }

  else if(tree_model %in% list(7,"crooked_alternate_tree")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[-(0)]1[+(0)](0),1[+(0)]1[-(0)](0)", "+-1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 5}

  }

  else if(tree_model %in% list(8,"crooked_arrow_weed")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[+(0)][-(0)](10)", "+-1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 5}

  }

  else if(tree_model %in% list(9,"crooked_twiggy_weed")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[-(0)]1[-(0)]+(0)", "+-1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 5}

  }

  else if(tree_model %in% list(10,"crooked_stochastic_fuzzy_weed")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1-[[(0)]+(0)]+1[+(10)]-(0),1+[[(0)]-(0)]-1[-(10)]+(0)","+-1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 4}

  }

  fractalforest::iterate_lsystem(init = '0', rules = rules, n_iter = n_iter)
}
