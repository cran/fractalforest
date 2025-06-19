#' Visualize a fractal tree
#'
#' This function generates a visualization of a fractal tree based on the geometry and attributes from the `build_tree` function.
#' The tree is plotted with branches and leaves. The user can customize the appearance using various parameters.
#'
#' @param tree_df A data frame resulting from the `build_tree` function, containing the tree's geometry and attributes.
#' @param d_col Name of the column that contains the diameters along the branches. Usually 'diameter'. Default is NULL (diameters are not used).
#' @param branch_color A character string specifying the color of the tree branches. Default is 'black'.
#' @param leaf_color A character string specifying the color of the tree leaves. Default is 'black'.
#' @param simplify A logical value. If `TRUE`, simplifies the geometries of the tree branches using a given tolerance. Default is `FALSE`.
#' @param dTolerance A numeric value specifying the tolerance parameter used when simplifying geometries, applicable if `simplify = TRUE`. Default is `0.15`.
#'
#' @return A ggplot object representing the tree profile.
#'
#' @export
plot_tree <- function(tree_df, d_col = NULL, branch_color = 'black', leaf_color = 'black', simplify = FALSE, dTolerance = .15){

  d_col <- rlang::enquo(d_col)

  if(rlang::quo_is_null(d_col)){
    tree_df <- tree_df %>%
      dplyr::mutate(diameter = .8)
  }
  else{
    d_col <- rlang::ensym(d_col)
    tree_df <- tree_df %>%
    dplyr::rename(diameter = {{d_col}})}

  df_geom <- tree_df[1:4] %>%
    dplyr::mutate(ID = dplyr::row_number()) %>%
    dplyr::select(5,1,3,2,4)

  rows <- df_geom %>%
    split(., seq(nrow(.)))

  lines <- lapply(rows, function(row) {
    lmat <- matrix(unlist(row[2:5]), ncol = 2, byrow = TRUE)
    sf::st_linestring(lmat)
  })
  lines <- sf::st_sfc(lines)
  lines_sf <- sf::st_sf('ID' = df_geom$ID, 'geometry' = lines)

  plot_df <- tree_df %>%
    dplyr::mutate(geometry = sf::st_buffer(lines_sf, dist = tree_df$diameter/100, endCapStyle = 'SQUARE') %>% dplyr::pull(geometry)) %>%
    sf::st_as_sf() %>%
    dplyr::group_by(type) %>%
    dplyr::summarise()

  if(simplify){
    tol <- dplyr::if_else(plot_df$type == 'branch', dTolerance, 0)
    plot_df <- plot_df %>%
      sf::st_simplify(dTolerance = tol)
  }

  ggplot2::ggplot()+
    ggplot2::geom_sf(ggplot2::aes(fill = type, color = type),
                 show.legend = FALSE, data = plot_df)+
    ggplot2::scale_fill_manual(values = c(branch_color, leaf_color))+
    ggplot2::scale_color_manual(values = c(branch_color, leaf_color))+
    ggplot2::theme_void()

}

