

testthat::test_that("Testing coords_to_node", {

  # vegnett <- vegnett_sampledata
  #
  # vegnett_list <- vegnett_to_R(vegnett = vegnett_sampledata,
  #                              year = 2021,
  #                              fromnodeID = "FROMNODEID",
  #                              tonodeID = "TONODEID",
  #                              FT_minutes = "FT_MINUTES",
  #                              TF_minutes = "TF_MINUTES",
  #                              meters = "SHAPE_LENGTH")
  #
  # nodes <- vegnett_list[[2]]
  # edges <- vegnett_list[[3]]

  from <- address_to_coords(zip_code = "0177",
                            address = "Akersveien 26")
  from_node <- coords_to_node(coords = from,
                              nodes_object = nodes,
                              edges_object = edges,
                              direction = "from")

  to <- address_to_coords(zip_code = "2211",
                          address = "Otervegen 23")
  to_node <- coords_to_node(coords = to,
                            nodes_object = nodes,
                            edges_object = edges,
                            direction = "to")

  testthat::expect_type(from_node, "list")
  testthat::expect_type(to_node, "list")

})

