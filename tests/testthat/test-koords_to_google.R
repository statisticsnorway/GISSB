
test_that("Tester koords_to_google", {

  fra <- adresse_api_koord(postnummer = "0177",
                           adresse = "Akersveien 26") %>%
    koords_to_google()

})
