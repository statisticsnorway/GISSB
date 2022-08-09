
test_that("Testing coords_to_google", {

  from <- address_to_coord(zip_code = "0177",
                          address = "Akersveien 26") %>%
    coords_to_google()

})
