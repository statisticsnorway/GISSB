test_that("Testing path_leaflet", {

  map <- path_leaflet(path)

  expect_type(map, "list")

})

