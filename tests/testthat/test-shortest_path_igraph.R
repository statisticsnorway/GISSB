
# OBS:

library(dplyr)

# Laster inn vegnett
vegnett <- arrow::open_dataset("X:/330/Helse-Individ-gruppe/2020-PA8-S-E-GEOS -WP2/GIS i R/Data/Vegnettverk/vegnett2021.parquet") %>%
  dplyr::filter(FYLKE_ID %in% c("3", "2", "4")) %>% # OBSOBSOBS
  sfarrow::read_sf_dataset()

vegnett_list <- vegnett_to_R(vegnett = vegnett)
graph <- vegnett_list[[1]]

testthat::test_that("Tester beregne_avstand", {

  distance_min <- shortest_path_igraph(from_node_ID = 26956,
                                       to_node_ID = 210373,
                                       unit = "FT_MINUTES")

  expect_type(distance_min, "list")

  distance_meter <- shortest_path_igraph(from_node_ID = 26956,
                                         to_node_ID = 210373,
                                         unit = "LENGTH")

  expect_type(distance_meter, "list")

  path <- shortest_path_igraph(from_node_ID = 26956,
                               to_node_ID = 210373,
                               unit = "FT_MINUTES",
                               path = T)

  expect_type(path, "list")

})








