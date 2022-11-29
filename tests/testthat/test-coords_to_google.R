
testthat::test_that("Testing coords_to_google", {

  from <- address_to_coords(zip_code = "0177",
                          address = "Akersveien 26") %>%
    coords_to_google()

  testthat::expect_type(from, "list")

})

