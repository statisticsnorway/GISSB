
# OBS:

# Laster inn vegnett
vegnett <- arrow::open_dataset("X:/330/Helse-Individ-gruppe/2020-PA8-S-E-GEOS -WP2/GIS i R/Data/Vegnettverk/vegnett2021.parquet") %>%
  dplyr::filter(FYLKE_ID %in% c("3", "2", "4")) %>%
  sfarrow::read_sf_dataset()

testthat::test_that("Testing vegnett_to_R", {

  vegnett_list <- vegnett_to_R(vegnett = vegnett)

  graph <- vegnett_list[[1]]
  nodes <- vegnett_list[[2]]
  edges <- vegnett_list[[3]]
  graph_cppRouting_FT_MINUTES <- vegnett_list[[4]]
  graph_cppRouting_LENGTH <- vegnett_list[[5]]


})
