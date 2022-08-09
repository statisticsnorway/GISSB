
#' Convert addresses to coordinates
#'
#' Function to find coordinates from the Norwegian Mapping Authority’s Cadastre (Norwegian: Matrikkelen, Norges eiendomsregister) through its address API.
#'
#' @param zip_code Character vector with zip codes.
#' @param address Character vector with addresses (street name and house number if available)
#' @param format Format of the returned data. Default value is set to “sf” (which returns an sf object). It is also possible set the format to “tibble” or “data.frame”.
#' @param crs_out Numeric vector with the chosen coordinate reference system (CRS). Default value is set to CRS 25833.
#'
#' @returns Object with coordinates to the specified addresses.
#' @export
#'
#' @examples
#' \dontrun{
#' from <- address_to_coord(zip_code = "0177",
#'                         address = "Akersveien 26")
#'                         }
#' @encoding UTF-8
#'
#'
address_to_coord <- function(zip_code,
                             address,
                             format = "sf",
                             crs_out = 25833) {

  if ((try(is.character(RCurl::getURL("https://ws.geonorge.no/adresser/v1/"))) == TRUE) == FALSE) {
    print("No access to https://ws.geonorge.no/adresser/v1/")
    stop()
  }


  # Loading packages #
  suppressWarnings(
    suppressPackageStartupMessages({
      library(tidyverse)
      library(httr)
      library(sf)
    }))

  # Funksjon for å hente lat og lon til gitt adresse #
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

  # Vektoriserer funksjon address_coord_func #
  address_coord_func_vec <- Vectorize(address_coord_func, vectorize.args = c("zip_code", "address"))

  # Bruker vektorene i funksjonen for å finne koordinatene til hver adresse #
  address_coord_alle <- data.frame()
  addresses_coord <- address_coord_func_vec(zip_code, address)

  # Gjør om til data frame #
  addresses_coord <- as.data.frame(t(addresses_coord))
  addresses_coord[] <- lapply(addresses_coord, unlist)

  addresses_coord$ID <- rep(1:nrow(addresses_coord))

  # Identifiserer adresser som mangler koordinater #
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
