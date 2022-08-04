
library(dplyr)

# Laster inn vegnett
vegnett <- arrow::open_dataset("X:/330/Helse-Individ-gruppe/2020-PA8-S-E-GEOS -WP2/GIS i R/Data/Vegnettverk/vegnett2021.parquet") %>%
  dplyr::filter(FYLKE_ID %in% c("3", "2", "4")) %>% # OBSOBSOBS
  sfarrow::read_sf_dataset()

vegnett_list <- vegnett_to_R(vegnett = vegnett)
graph <- vegnett_list[[1]]

testthat::test_that("Tester beregne_avstand", {

  avstand_min <- beregne_avstand(from_node = 26956,
                                 to_node = 210373,
                                 enhet = "FT_MINUTES")

  expect_type(avstand_min, "list")

  avstand_meter <- beregne_avstand(from_node = 26956,
                                   to_node = 210373,
                                   enhet = "LENGTH")

  expect_type(avstand_meter, "list")

  path <- beregne_avstand(from_node = 26956,
                          to_node = 210373,
                          enhet = "FT_MINUTES",
                          path = T)

  expect_type(avstand_meter, "list")

})
