  test_that("Testing coords_to_node", {

  from <- address_to_coords(zip_code = "0177",
                           address = "Akersveien 26")
  from_node <- coords_to_node(coords = from, direction = "from")

  to <- adresse_api_koord(zip_code = "2211",
                          address = "Otervegen 23")
  to_node <- address_to_coords(coords = to, direction = "to")


  })
