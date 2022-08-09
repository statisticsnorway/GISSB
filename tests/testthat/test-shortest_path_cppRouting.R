
# OBS:

library(dplyr)

vegnett <- arrow::open_dataset("X:/330/Helse-Individ-gruppe/2020-PA8-S-E-GEOS -WP2/GIS i R/Data/Vegnettverk/vegnett2021.parquet") %>%
  dplyr::filter(FYLKE_ID %in% c("3", "2", "4")) %>% # OBSOBSOBS
  sfarrow::read_sf_dataset()

vegnett_list <- vegnett_to_R(vegnett = vegnett)
graph_cppRouting_FT_MINUTES <- vegnett_list[[4]]
graph_cppRouting_LENGTH <- vegnett_list[[5]]

testthat::test_that("Testing shortest_paths_cppRouting", {

  distance_cpp_min <- shortest_paths_cppRouting(26956,
                                                210373,
                                                enhet = "FT_MINUTES")

  expect_type(distance_cpp_min, "list")

  distance_cpp_meter <- shortest_paths_cppRouting(26956,
                                                  210373,
                                                  enhet = "LENGTH")

  expect_type(distance_cpp_meter, "list")



})

