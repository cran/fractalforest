#' Generate a Point Cloud from 2D trees data frame
#'
#' This function generates a point cloud based on the geometry of a tree or forest profile.
#'
#' @param tree_df A data.frame resulting from the `build_tree` or `build_forest_profile` functions. It contains the tree geometry data.
#' @param pt_distance A numeric value specifying the distance between points in the generated point cloud. The default is 0.3. Smaller values will result in a denser point cloud. Small values may significantly increase the execution time of the function due to the larger number of points being generated.
#'
#' @details This function processes the input `tree_df` by creating lines from the tree's geometry and then buffers these lines to account for the tree's diameter. A grid is then created over the buffered geometry, and points are generated where the grid intersects the geometry. The result is a point cloud of 2D coordinates representing the tree's profile.
#'
#' @return A matrix of 2D coordinates representing the point cloud. Each row corresponds to a point in the cloud, with the columns representing the X and Y coordinates.
#'
#' @export
generate_point_cloud <- function(tree_df, pt_distance = .3){

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
    dplyr::mutate(geometry = sf::st_buffer(lines_sf, dist = tree_df$diameter/100, endCapStyle = 'SQUARE') %>% dplyr::pull(geometry),
                  h_lin = sqrt((from_x-to_x)^2+(from_y-to_y)^2)) %>%
    sf::st_as_sf()

  grid <- sf::st_make_grid(plot_df, cellsize = pt_distance, square = TRUE)
  pts <- grid %>% sf::st_cast('POINT')
  pts_itsct <- sf::st_intersects(pts, plot_df, sparse = FALSE)
  point_cloud <- pts[rowSums(pts_itsct)>=1]
  pt_cloud_xy <- sf::st_coordinates(point_cloud)
  return(pt_cloud_xy)

}
