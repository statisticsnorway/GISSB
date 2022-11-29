

testthat::test_that("Tester beregne_avstand", {

  vegnett <- vegnett_sampledata

  vegnett_list <- vegnett_to_R(vegnett = vegnett_sampledata,
                               year = 2021,
                               fromnodeID = "FROMNODEID",
                               tonodeID = "TONODEID",
                               FT_minutes = "FT_MINUTES",
                               TF_minutes = "TF_MINUTES",
                               meters = "SHAPE_LENGTH")

  graph <- vegnett_list[[1]]

  distance_min <- shortest_path_igraph(from_node_ID = 54,
                                       to_node_ID = 61,
                                       graph_object = graph,
                                       unit = "minutes")

  testthat::expect_type(distance_min, "list")

  distance_meter <- shortest_path_igraph(from_node_ID = 54,
                                         to_node_ID = 61,
                                         graph_object = graph,
                                         unit = "meters")

  testthat::expect_type(distance_meter, "list")

  path <- shortest_path_igraph(from_node_ID = 54,
                               to_node_ID = 61,
                               graph_object = graph,
                               unit = "minutes",
                               path = T)

  testthat::expect_type(path, "list")

})








