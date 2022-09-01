
# address_to_coords <- function(zip_code,
#                               address,
#                               format = "sf",
#                               crs_out = 25833) {
#
#   if ((try(is.character(RCurl::getURL("https://ws.geonorge.no/adresser/v1/"))) == TRUE) == FALSE) {
#     print("No access to: https://ws.geonorge.no/adresser/v1/")
#     stop()
#   }

  # Function to get lat and lon til supplied address #
  address_coord_func <- function(zip_code, address){
    resp <- httr::GET(paste0("https://ws.geonorge.no/adresser/v1/sok?",
                             "postnummer=", zip_code, "&",
                             "adressetekst=", "'", gsub(" ", "+", address), "'"))
    cont_raw <- httr::content(resp)

    if (cont_raw$metadata$totaltAntallTreff == 1) {
      lat <- cont_raw$adresser[[1]]$representasjonspunkt$lat
      lon <- cont_raw$adresser[[1]]$representasjonspunkt$lon
      kommunenummer <- cont_raw$adresser[[1]]$kommunenummer
      kommunenavn <- cont_raw$adresser[[1]]$kommunenavn
    } else {
      lat <- NA
      lon <- NA
      kommunenummer <- NA
      kommunenavn <- NA
      kommunenavn <- NA
    }


    address_coord <- data.frame(address, zip_code, lon, lat, kommunenummer, kommunenavn)
    return(address_coord_alle <- rbind(address_coord_alle, address_coord))
  }

  # Vectorize the address_coord_func function #
  address_coord_func_vec <- Vectorize(address_coord_func, vectorize.args = c("zip_code", "address"))

  # Use the vectorized function to find the coordinates to each address
  address_coord_alle <- data.frame()
  addresses_coord <- address_coord_func_vec(zip_code = c("8430", "9700"),
                                            address = c("Storgata 52", "Helsetunvegen 2"))

  # Convert to data.frame #
  addresses_coord <- as.data.frame(t(addresses_coord))

  # Legger til A
  addresses_coord_failed <- addresses_coord %>%
    dplyr::filter(is.na(lat)) %>%
    dplyr::mutate(nummer = as.numeric(str_sub(address,-1,-1)),
                  address_added_A = case_when(
                    nummer %in% c(1:9) ~ paste0(address, "A"),
                    TRUE ~ ""),
                  address_gata_gaten = gsub("gata", "gaten", address),
                  address_gata_gaten = gsub("gaten", "gata", address),
                  address_vegen_veien = gsub("vegen", "veien", address),
                  address_veien_vegen = gsub("veien", "vegen", address))

      # address_new = case_when(
      # nummer = as.numeric(str_sub(address,-1,-1)) # %in% 1:9 ~ paste0(address, "A"),
      # TRUE ~ address))

  zip_failed <- unique(addresses_coord_failed$zip_code)
  address_added_A <- unique(addresses_coord_failed$address_added_A)

  address_coord_alle <- data.frame()
  addresses_coord <- address_coord_func_vec(zip_code = zip_failed,
                                            address = address_added_A)

  # Convert to data.frame #
  addresses_coord <- as.data.frame(t(addresses_coord))






  # if (as.numeric(str_sub(address_failed,-1,-1)) %in% 1:9){
  #   address_failed <- paste0(address_failed, "A")
  # }

  addresses_coord[] <- lapply(addresses_coord, unlist)

  addresses_coord$ID <- rep(1:nrow(addresses_coord))

  # Identify addresses that are missing coordinates #
  addresses_coord_missing <- addresses_coord %>%
    dplyr::filter(is.na(lat))

  if (format == "sf" & nrow(addresses_coord_missing)==0) {
    addresses_coord <- addresses_coord %>%
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4258) %>%
      sf::st_transform(crs = crs_out)
  }

  if (format == "tibble") {
    addresses_coord <- addresses_coord %>%
      as_tibble()
  }

  return(addresses_coord)

}


address_to_coords(zip_code = "8430",
                  address = "Storgata 52A")

