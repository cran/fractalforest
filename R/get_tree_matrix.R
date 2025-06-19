#' Extract the Fractal Tree Matrix from a Tree Data Frame
#'
#' @param tree_df A data frame containing the tree data, typically resulting from the `build_tree` function. The data frame should include columns `from_x`, `from_y`, `to_x`, and `to_y`, which represent the coordinates of the tree segments.
#'
#' @return A matrix of unique coordinates (x, y) representing the positions of the tree's segments.
#' @details This function extracts the start and end coordinates of the tree segments and returns the result as a matrix.
#'
#' @export
#'
get_tree_matrix <- function(tree_df){

  tree_df %>%
    dplyr::select(x = from_x, y = from_y) %>%
    dplyr::bind_rows(
      tree_df %>%
        dplyr::select(x = to_x, y = to_y)
    ) %>%
    dplyr::distinct() %>%
    as.matrix() %>%
    unname()

}
