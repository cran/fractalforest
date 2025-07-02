## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fractalforest)

## ---- eval = FALSE------------------------------------------------------------
#  # install.packages('devtools')
#  library(devtools)
#  install_github('sergiocostafh/fractalforest')

## -----------------------------------------------------------------------------
library(fractalforest)

## -----------------------------------------------------------------------------
binary_tree_rules <- data.frame(inp = c("0", "1"),
                         out = c("1[-0]+0", "1"), stringsAsFactors = FALSE)

## -----------------------------------------------------------------------------
tree_string <- iterate_lsystem(init = "0", rules = binary_tree_rules, n = 5)
tree_string

## -----------------------------------------------------------------------------
tree <- build_tree(string = tree_string, angle = 15)
head(tree)

## ---- results='hide', out.width='100%', fig.width=7, fig.height=5, dpi = 200----
plot_tree(tree)

## ---- results='hide', out.width='100%', fig.width=7, fig.height=5, dpi = 200----
binary_tree_rules <- data.frame(inp = c("0", "1"),
                         out = c("1[-(0)]+(0)", "1"), stringsAsFactors = FALSE)
tree_string <- iterate_lsystem(init = "0", rules = binary_tree_rules, n = 5)
tree <- build_tree(string = tree_string, angle = 15)
plot_tree(tree)

## ---- message = F, warning=FALSE, results='hide', out.width='100%', fig.width=7, fig.height=5, dpi = 200----
library(ggplot2)
library(patchwork)
library(dplyr)

trees_n <- lapply(1:8, 
                  function(x) {
                    iterate_lsystem(init = '0', rules = binary_tree_rules, n = x) %>% 
                      build_tree(string = ., angle = 15) %>% 
                      plot_tree()+
                      labs(title = paste0('n = ', x))
  })

wrap_plots(trees_n, nrow = 2, heights = c(1,1))

## ---- message = F, warning=FALSE, results='hide', out.width='100%', fig.width=7, fig.height=5, dpi = 200----
alternate_bush_rules <- data.frame(inp = c("0", "1"),
                          out = c("1[+1][--(0)]+1-1[+++(0)]-(0)", "1"), stringsAsFactors = FALSE)
tree_string <- iterate_lsystem(init = "0", rules = alternate_bush_rules, n = 6)
tree1 <- build_tree(string = tree_string, angle = 10) %>% 
  plot_tree()+
  labs(title = 'alternate tree') 

arrow_weed_rules <- data.frame(inp = c("0", "1"),
                          out = c("1[+(0)][-(0)](10)", "1"), stringsAsFactors = FALSE)
tree_string <- iterate_lsystem(init = "0", rules = arrow_weed_rules, n = 6)
tree2 <- build_tree(string = tree_string, angle = 30) %>% 
  plot_tree()+
  labs(title = 'arrow weed')

twiggy_weed_rules <- data.frame(inp = c("0", "1"),
                          out = c("1[-(0)]1[-(0)]+(0)", "1"), stringsAsFactors = FALSE)
tree_string <- iterate_lsystem(init = "0", rules = twiggy_weed_rules, n = 6)
tree3 <- build_tree(string = tree_string, angle = 25) %>% 
  plot_tree()+
  labs(title = 'twiggy weed')

tree1 + tree2 + tree3

## ---- message = F, warning=FALSE, results='hide', out.width='100%', fig.width=7, fig.height=5, dpi = 200----
fractal_tree_model(5) %>% 
  build_tree() %>% 
  plot_tree()

## ---- include = FALSE, message = F, warning=FALSE, results='hide', out.width='100%', fig.width=7, fig.height=5, dpi = 200----

for(i in 1:10){
  if(i > 5){rdns = TRUE}
  else{rdns = FALSE}
    t <- fractal_tree_model(i) %>% 
  build_tree(randomness = rdns) %>% 
  mutate(model = i)
  if(i == 1){
    ts <- t
  } else{
    ts <- bind_rows(ts, t)
  }
    
}

ts <- ts %>% 
  mutate(model = paste0('model = ',model))
ts <- ts %>% 
  mutate(model = factor(model, levels = unique(ts$model), ordered = T))

library(sf)

  df_geom <- ts[1:4] %>%
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

  plot_df <- ts %>%
    dplyr::mutate(geometry = sf::st_buffer(lines_sf, dist = .00001, endCapStyle = 'SQUARE') %>% dplyr::pull(geometry)) %>%
    sf::st_as_sf() %>%
    dplyr::group_by(type, model) %>%
    dplyr::summarise()

  ggplot2::ggplot()+
    ggplot2::geom_sf(data = plot_df)+
    ggplot2::theme_void()+
    facet_wrap(~model, nrow = 2)

## ---- message = F, warning=FALSE, results='hide', out.width='100%', fig.width=10, fig.height=5, dpi = 200----
tree1 <- build_tree(string = tree_string, angle = 15, h_reduction = .7) %>% 
  plot_tree()+
  labs(title = 'h_reduction = .7')

tree2 <- build_tree(string = tree_string, angle = 15) %>% 
    plot_tree()+
  labs(title = 'h_reduction = .61803 (default)')
  
tree3 <- build_tree(string = tree_string, angle = 15, h_reduction = .5) %>% 
    plot_tree()+
  labs(title = 'h_reduction = .5')

tree1 + tree2 + tree3


## ---- message = F, warning=FALSE, results='hide', out.width='100%', fig.width=10, fig.height=7, dpi = 200----
tree1 <- build_tree(string = tree_string, angle = 15, randomness = TRUE) %>% 
  plot_tree()+
  labs(title = 'length_cv = .1 (default)')

tree2 <- build_tree(string = tree_string, angle = 15, randomness = TRUE, length_cv = .5) %>% 
  plot_tree()+
  labs(title = 'length_cv = .5')
  
tree3 <- build_tree(string = tree_string, angle = 15, randomness = TRUE) %>% 
  plot_tree()+
  labs(title = 'angle_cv = .1 (default)')

tree4 <- build_tree(string = tree_string, angle = 15, randomness = TRUE, angle_cv = .5) %>% 
  plot_tree()+
  labs(title = 'angle_cv = .5')

(tree1 + tree2) / (tree3 + tree4)

## ---- message = F, warning=FALSE, results='hide', out.width='100%', fig.width=7, fig.height=5, dpi = 200----
tree <- build_tree(string = tree_string, angle = 15, height = 10)
plot_tree(tree)+
  labs(x = '', y = 'Height (m)')+
  theme_bw() 

## ---- message = F, warning=FALSE, results='hide', out.width='100%', fig.width=10, fig.height=5, dpi = 200----
p1 <- build_tree(string = tree_string, angle = 15, height = 10, diameter = 20) %>% 
  plot_tree(d_col = diameter)+
  labs(title = 'leaf_size = NULL (default)')

p2 <- build_tree(string = tree_string, angle = 15, height = 10, diameter = 20, leaf_size = 10) %>% 
  plot_tree(d_col = diameter,)+
  labs(title = 'leaf_size = 10')

p3 <- build_tree(string = tree_string, angle = 15, height = 10, diameter = 20, leaf_size = 20) %>% 
  plot_tree(d_col = diameter)+
  labs(title = 'leaf_size = 20')

p1 + p2 + p3

## ---- message = F, warning=FALSE, results='hide', out.width='100%', fig.width=10, fig.height=5, dpi = 200----
tree1 <- build_tree(string = tree_string, angle = 15, height = 5, diameter = 7, d_reduction = .7) %>% 
  plot_tree(d_col = diameter)+
  labs(title = 'd_reduction = .7')

tree2 <- build_tree(string = tree_string, angle = 15, height = 5, diameter = 7) %>% 
  plot_tree(d_col = diameter)+
  labs(title = 'd_reduction = .61803 (default)')

tree3 <- build_tree(string = tree_string, angle = 15, height = 5, diameter = 7, d_reduction = .5) %>% 
  plot_tree(d_col = diameter)+
  labs(title = 'd_reduction = .5')

tree1 + tree2 + tree3

## ---- message = F, warning=FALSE, results='hide', out.width='100%', fig.width=7, fig.height=5, dpi = 200----
library(patchwork)
tree_string <- iterate_lsystem(init = "0", rules = binary_tree_rules, n = 10)
tree <- build_tree(string = tree_string, angle = 15, height = 10, diameter = 20)

plot_tree(tree, d_col = diameter, branch_color = 'lightsalmon4', leaf_color = 'darkgreen')

