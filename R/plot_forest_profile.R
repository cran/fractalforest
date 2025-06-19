#' Visualize the 2D profile of a fractal trees forest
#'
#' This function generates a 2D visualization of the forest profile using data from a fractal trees.
#' The forest is plotted with tree branches and leaves. The user can customize the appearance using different parameters.
#'
#' @param tree_df A data frame resulting from the `build_forest_profile` function, containing the geometry and attributes of the trees.
#' @param d_col Name of the column that contains the diameters along the branches. Usually 'diameter'. Default is NULL (diameters are not used).
#' @param branch_color A character string specifying the color of the tree branches. Default is 'black'.
#' @param leaf_color A character string specifying the color of the tree leaves. Default is 'black'.
#' @param label A character string or column name specifying the labels for the trees (typically placed along the x-axis). Default is `NULL`.
#' @param label_size A numeric value specifying the size of the labels on the plot. Default is `4`.
#' @param simplify A logical value. If `TRUE`, simplifies the geometries of the tree branches using a given tolerance. Default is `FALSE`.
#' @param dTolerance A numeric value specifying the tolerance parameter used when simplifying geometries, applicable if `simplify = TRUE`. Default is `0.15`.
#'
#' @return A ggplot object representing the forest profile.
#'
#' @export
plot_forest_profile <- function(tree_df, d_col = NULL, branch_color = 'black', leaf_color = 'black', label = NULL, label_size = 4, simplify = FALSE, dTolerance = .15){

  d_col <- rlang::enquo(d_col)
  branch_color <- rlang::enquo(branch_color)
  leaf_color <- rlang::enquo(leaf_color)
  label <- rlang::enquo(label)

  if(rlang::quo_is_null(d_col)){
    tree_df <- tree_df %>%
      dplyr::mutate(diameter = .8)
  }
  else if(!dplyr::as_label(d_col) %in% colnames(tree_df)){
    d_col <- rlang::ensym(d_col)
    tree_df <- tree_df %>%
      dplyr::rename(diameter = {{d_col}})}

  if(rlang::quo_is_symbol(branch_color)){

    branch_color <- rlang::ensym(branch_color)

    tree_df <- tree_df %>%
      dplyr::mutate(bc = !!branch_color)
  }
  else{

    branch_color <- branch_color %>% rlang::as_name()

    tree_df <- tree_df %>%
      dplyr::mutate(bc = branch_color)
  }

  if(rlang::quo_is_symbol(leaf_color)){

    leaf_color <- rlang::ensym(leaf_color)

    tree_df <- tree_df %>%
      dplyr::mutate(lc = !!leaf_color)
  }
  else{

    leaf_color <- leaf_color %>% rlang::as_name()

    tree_df <- tree_df %>%
      dplyr::mutate(lc = leaf_color)

  }

  if(rlang::quo_is_null(label)){
    tree_df <- tree_df %>%
      dplyr::mutate(label = NA_character_)
  }
  else{
    label <- rlang::ensym(label)
    tree_df <- tree_df %>%
      dplyr::mutate(label = {{label}})}

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
    dplyr::group_by(type, lc, bc) %>%
    dplyr::summarise()

  if(simplify){
    tol <- dplyr::if_else(plot_df$type == 'branch', dTolerance, 0)
    plot_df <- plot_df %>%
      sf::st_simplify(dTolerance = tol)
  }

  tree_df_label <- tree_df %>%
    filter(from_y == 0) %>%
    group_by(tree_id, label) %>%
    summarise(xlabel = from_x)

  ggplot2::ggplot()+
    ggplot2::geom_sf(ggplot2::aes(fill = bc, color = bc),
                     show.legend = FALSE, data = plot_df %>% dplyr::filter(type == 'branch'))+
    ggplot2::geom_sf(ggplot2::aes(fill = lc, color = lc),
                     show.legend = FALSE, data = plot_df %>% dplyr::filter(type == 'leaf'))+
    ggplot2::scale_fill_identity()+
    ggplot2::scale_color_identity()+
    ggplot2::labs(y = 'Height (m)')+
    ggplot2::scale_y_continuous(limits=function(x){c(0, max(x)*1.1)}, expand = c(0,0), breaks = seq(0,100,5))+
    ggplot2::scale_x_continuous(breaks = tree_df_label$xlabel, labels = tree_df_label$label)+
    cowplot::theme_minimal_hgrid()+
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
          panel.grid = ggplot2::element_line(linetype = 'dashed'),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 45, size = 8, hjust = 1))

}

