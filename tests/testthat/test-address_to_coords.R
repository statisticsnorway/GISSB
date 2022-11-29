

testthat::test_that("Testing address_to_coords", {
  coords <- address_to_coords(zip_code = "0177", address = "Akersveien 26")
  testthat::expect_type(coords, "list")

})


