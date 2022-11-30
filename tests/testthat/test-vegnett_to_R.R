
# OBS:


testthat::test_that("Testing vegnett_to_R", {

  vegnett <- vegnett_sampledata


  vegnett_list <- vegnett_to_R(vegnett = vegnett_sampledata,
                               year = 2021,
                               fromnodeID = "FROMNODEID",
                               tonodeID = "TONODEID",
                               FT_minutes = "FT_MINUTES",
                               TF_minutes = "TF_MINUTES",
                               meters = "SHAPE_LENGTH")

  graph <- vegnett_list[[1]]
  nodes <- vegnett_list[[2]]
  edges <- vegnett_list[[3]]
  graph_cppRouting_minutes <- vegnett_list[[4]]
  graph_cppRouting_meters <- vegnett_list[[5]]

  testthat::expect_type(graph, "list")
  testthat::expect_type(nodes, "list")
  testthat::expect_type(edges, "list")
  testthat::expect_type(graph_cppRouting_minutes, "list")
  testthat::expect_type(graph_cppRouting_meters, "list")


})
