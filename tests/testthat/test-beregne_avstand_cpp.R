
library(dplyr)

# Laster inn vegnett
vegnett <- arrow::open_dataset("X:/330/Helse-Individ-gruppe/2020-PA8-S-E-GEOS -WP2/GIS i R/Data/Vegnettverk/vegnett2021.parquet") %>%
  dplyr::filter(FYLKE_ID %in% c("3", "2", "4")) %>% # OBSOBSOBS
  sfarrow::read_sf_dataset()

vegnett_list <- vegnett_to_R(vegnett = vegnett)
graph_cppRouting_FT_MINUTES <- vegnett_list[[4]]
graph_cppRouting_LENGTH <- vegnett_list[[5]]

testthat::test_that("Tester beregne_avstand", {

  avstand_cpp_min <- beregne_avstand_cpp(26956,
                                         210373,
                                        enhet = "FT_MINUTES")

  expect_type(avstand_cpp_min, "list")

  avstand_cpp_meter <- beregne_avstand_cpp(26956,
                                           210373,
                                           enhet = "LENGTH")

  expect_type(avstand_cpp_meter, "list")



})

