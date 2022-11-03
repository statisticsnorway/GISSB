
# OBS: legg til fix der to tall finnes med bindestrek i adressen (f.eks. 25-27)


#' Convert addresses to coordinates
#'
#' `address_to_coords` is a function to find coordinates to supplied Norwegian addresses. Internet access is required as the function utilizes \href{https://ws.geonorge.no/adresser/v1/}{the Norwegian Mapping Authority’s address API}.
#'
#' @param zip_code Character vector with zip codes.
#' @param address Character vector with addresses (street name and house number).
#' @param format Format of the returned object. Default value is set to “sf” (which returns an sf object). It is also possible set the format to “tibble” or “data.frame”.
#' @param crs_out Numeric vector with the chosen coordinate reference system (CRS). Default value is set to CRS 25833.
#'
#' @returns Object with coordinates to the supplied addresses.
#' @export
#'
#' @examples
#' address_to_coords(zip_code = "0177", address = "Akersveien 26")
#'
#' @encoding UTF-8
#'
#'
address_to_coords_new <- function(zip_code,
                              address,
                              format = "sf",
                              crs_out = 25833) {

  if ((try(is.character(RCurl::getURL("https://ws.geonorge.no/adresser/v1/"))) == TRUE) == FALSE) {
    print("No access to: https://ws.geonorge.no/adresser/v1/")
    stop()
  }

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
  addresses_coord <- address_coord_func_vec(zip_code, address)

  # Convert to data.frame #
  addresses_coord <- as.data.frame(t(addresses_coord))
  addresses_coord[] <- lapply(addresses_coord, unlist)

  # Fix
  addresses_coord_failed <- addresses_coord %>%
    dplyr::filter(is.na(lat))


  adress_fix <- function(addresses_coord_failed){

    if (nrow(addresses_coord_failed) > 0) {
      addresses_coord_failed <- addresses_coord_failed %>%
        dplyr::mutate(nummer = as.numeric(str_sub(address,-1,-1)),
                      address_added_A = case_when(
                        nummer %in% c(1:9) ~ paste0(address, "A"),
                        TRUE ~ ""),
                      address_gata_gaten = gsub("gata", "gaten", address),
                      address_gaten_gata = gsub("gaten", "gata", address),
                      address_vegen_veien = gsub("vegen", "veien", address),
                      address_veien_vegen = gsub("veien", "vegen", address))

      zip_failed <- unique(addresses_coord_failed$zip_code)

      address_added_A <- unique(addresses_coord_failed$address_added_A)
      address_gata_gaten <- unique(addresses_coord_failed$address_gata_gaten)
      address_gaten_gata <- unique(addresses_coord_failed$address_gaten_gata)
      address_vegen_veien <- unique(addresses_coord_failed$address_vegen_veien)
      address_veien_vegen <- unique(addresses_coord_failed$address_veien_vegen)

      addresses_coord_2 <- address_coord_func_vec(zip_code = zip_failed,
                                                  address = address_added_A)

      addresses_coord_2 <- as.data.frame(t(addresses_coord_2)) %>%
        dplyr::filter(!is.na(lon))


      if (nrow(addresses_coord_2) != nrow(addresses_coord_failed)) {
        addresses_coord_3 <- address_coord_func_vec(zip_code = zip_failed,
                                                    address = address_gaten_gata)

        addresses_coord_3 <- as.data.frame(t(addresses_coord_3)) %>%
          dplyr::filter(!is.na(lon))
        addresses_coord_2 <- rbind(addresses_coord_2, addresses_coord_3)
      }



      if (nrow(addresses_coord_2) != nrow(addresses_coord_failed)) {
        addresses_coord_4 <- address_coord_func_vec(zip_code = zip_failed,
                                                    address = address_vegen_veien)

        addresses_coord_4 <- as.data.frame(t(addresses_coord_4)) %>%
          dplyr::filter(!is.na(lon))
        addresses_coord_2 <- rbind(addresses_coord_2, addresses_coord_4)

      }



      if (nrow(addresses_coord_2) != nrow(addresses_coord_failed)) {
        addresses_coord_5 <- address_coord_func_vec(zip_code = zip_failed,
                                                    address = address_veien_vegen)

        addresses_coord_5 <- as.data.frame(t(addresses_coord_5)) %>%
          dplyr::filter(!is.na(lon))
        addresses_coord_2 <- rbind(addresses_coord_2, addresses_coord_5)

      }

      if (nrow(addresses_coord_2) != nrow(addresses_coord_failed)) {
        addresses_coord_2 <- addresses_coord_failed %>%
          dplyr::select(-address_added_A, -address_gata_gaten, -address_gaten_gata, -address_vegen_veien, -address_veien_vegen)

      }
    }


    return(addresses_coord_2)

  }


addresses_coord_2 <-  adress_fix(addresses_coord_failed)


addresses_coord_not_missing <- addresses_coord %>%
  dplyr::filter(!is.na(lat))

addresses_coord_3 <- rbind(addresses_coord_not_missing, addresses_coord_2)

addresses_coord_3$ID <- rep(1:nrow(addresses_coord_3))

# Identify addresses that are missing coordinates #
addresses_coord_missing <- addresses_coord_3 %>%
  dplyr::filter(is.na(lat))

if (format == "sf" & nrow(addresses_coord_missing)==0) {
  addresses_coord_3 <- addresses_coord_3 %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4258) %>%
    sf::st_transform(crs = crs_out)
}

if (format == "tibble") {
  addresses_coord_3 <- addresses_coord_3 %>%
    as_tibble()
}



return(addresses_coord_3)

}
