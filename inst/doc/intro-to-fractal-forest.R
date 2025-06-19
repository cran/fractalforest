## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE, results='hide', out.width='100%', fig.width=7, fig.height=5, dpi = 200----
library(fractalforest)
library(forestmangr)
library(dplyr)

data("exfm20")

dados <- exfm20 %>%
  filter(!dead)

fracflor <- build_forest_profile(data = dados,
                                 height = Htot,
                                 diameter = dbh,
                                 label = scientific.name,
                                 leaf_size = 50,
                                 tree_model = 2,
                                 sample = TRUE,
                                 n_trees = 30,
)

plot_forest_profile(fracflor,
                    label = scientific.name)

## ---- results='hide', out.width='100%', fig.width=7, fig.height=5, dpi = 200----
fracflor_colors <- fracflor %>%
  select(tree_id) %>%
  distinct() %>%
  rowwise() %>%
  mutate(cor_folhas = sample(c('darkgreen','darkolivegreen','darkolivegreen4','darkseagreen4'), 1),
         cor_ramos = sample(c('firebrick4','brown4','chocolate4','bisque4'),1))

fracflor2 <- fracflor %>%
  left_join(fracflor_colors, by = 'tree_id')

plot_forest_profile(fracflor2,
                    d_col = diameter,
                    branch_color = cor_ramos,
                    leaf_color = cor_folhas,
                    label = scientific.name)

## ---- results='hide', out.width='100%', fig.width=7, fig.height=5, dpi = 200----

fracflor <- build_forest_profile(data = dados,
                                 height = Htot,
                                 diameter = dbh,
                                 label = scientific.name,
                                 leaf_size = 50,
                                 randomness = T,
                                 angle_cv = .1,
                                 length_cv = .3,
                                 dist_cv = .2,
                                 tree_model = 6,
                                 sample = TRUE,
                                 n_trees = 30,
)

fracflor2 <- fracflor %>%
  left_join(fracflor_colors, by = 'tree_id')

plot_forest_profile(fracflor2,
                    d_col = diameter,
                    branch_color = cor_ramos,
                    leaf_color = cor_folhas,
                    label = scientific.name)

