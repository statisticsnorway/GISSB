
testthat::test_that("Testing shortest_paths_cppRouting", {

  vegnett <- vegnett_sampledata

  vegnett_list <- vegnett_to_R(vegnett = vegnett_sampledata,
                               year = 2021,
                               fromnodeID = "FROMNODEID",
                               tonodeID = "TONODEID",
                               FT_minutes = "FT_MINUTES",
                               TF_minutes = "TF_MINUTES",
                               meters = "SHAPE_LENGTH")

  graph_cppRouting_minutes <- vegnett_list[[4]]
  graph_cppRouting_meters <- vegnett_list[[5]]

  distance_cpp_min <- shortest_path_cppRouting(54,
                                               61,
                                               unit = "minutes",
                                               graph_cppRouting_object = graph_cppRouting_minutes)

  testthat::expect_type(distance_cpp_min, "list")

  distance_cpp_meter <- shortest_path_cppRouting(54,
                                                 61,
                                                 unit = "meters",
                                                 graph_cppRouting_object = graph_cppRouting_meters)

  testthat::expect_type(distance_cpp_meter, "list")


})

