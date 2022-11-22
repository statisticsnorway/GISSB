
#' Convert addresses to coordinates
#'
#' The function `address_to_coords` can be used to find coordinates to supplied Norwegian addresses. Internet access is required as the function utilizes \href{https://ws.geonorge.no/adresser/v1/}{the Norwegian Mapping Authority’s address API}.
#'
#' If there are no coordinates found for the supplied address it means that it does not exist in [Matrikkelen](https://www.kartverket.no/eiendom/eiendomsgrenser/matrikkelen-norgeseiendomsregister) - Norway's official property register. See \href{https://www.rettikartet.no/app/veger} to search for existing addresses.
#'
#' @param zip_code Character vector with zip codes.
#' @param address Character vector with addresses (street name and house number).
#' @param format Format of the returned object. Default value is set to “sf” (which returns an `sf` object). It is also possible set the format to “tibble” or “data.frame”.
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
address_to_coords <- function(zip_code,
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
