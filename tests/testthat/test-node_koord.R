test_that(

  fra <- adresse_api_koord(postnummer = "0177",
                           adresse = "Akersveien 26")
  from_node <- node_koord(koords = fra, fra_til = "fra")

  til <- adresse_api_koord(postnummer = "2211",
                           adresse = "Otervegen 23")
  to_node <- node_koord(koords = til, fra_til = "til")


)
