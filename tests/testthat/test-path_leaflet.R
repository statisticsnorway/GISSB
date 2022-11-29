
vegnett <- vegnett_sampledata

vegnett_list <- vegnett_to_R(vegnett = vegnett_sampledata,
                             year = 2021,
                             fromnodeID = "FROMNODEID",
                             tonodeID = "TONODEID",
                             FT_minutes = "FT_MINUTES",
                             TF_minutes = "TF_MINUTES",
                             meters = "SHAPE_LENGTH")

graph <- vegnett_list[[1]]

testthat::test_that("Testing path_leaflet", {

  path <- shortest_path_igraph(from_node_ID = 54,
                               to_node_ID = 61,
                               unit = "minutes",
                               path = T)
  map <- path_leaflet(path)

  testthat::expect_type(map, "list")

})

