test_that("Tester path_leaflet", {

  map <- path_leaflet(path)

  expect_type(map, "list")

})

