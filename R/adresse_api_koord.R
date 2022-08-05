
#' Adresser til koordinater
#'
#' Funksjon for å finne koordinater fra matrikkelen (adresse-API fra Kartverket).
#'
#' @param postnummer Karaktervektor med postnummer.
#' @param adresse Karaktervektor med gateadresse (og gatenummer hvis tilgjengelig).
#' @param format Format på datasettet som returneres. Default er satt til "sf" (sf-objekt), men det er også mulig å sette format = "tibble" eller "data.frame".
#' @param crs Numerisk vektor med ønsket koordinatsystem for koordinatene. Default er satt til CRS 25833.
#'
#' @returns Objekt med koordinatene til adressene.
#' @export
#'
#' @examples
#' fra <- adresse_api_koord(postnummer = "0177",
#'                         adresse = "Akersveien 26")
#' @encoding UTF-8
#'
#'
adresse_api_koord <- function(postnummer,
                              adresse,
                              format = "sf",
                              crs = 25833) {

  if ((try(is.character(RCurl::getURL("https://ws.geonorge.no/adresser/v1/"))) == TRUE) == FALSE) {
    print("Mangler tilgang til https://ws.geonorge.no/adresser/v1/")
    stop()
  }


  # Laster inn pakker #
  suppressWarnings(
    suppressPackageStartupMessages({
      library(tidyverse)
      library(httr)
      library(sf)
    }))

  # Funksjon for å hente lat og lon til gitt adresse #
  adresse_koord_funk <- function(postnummer, adresse){
    resp <- httr::GET(paste0("https://ws.geonorge.no/adresser/v1/sok?",
                             "postnummer=", postnummer, "&",
                             "adressetekst=", "'", gsub(" ", "+", adresse), "'"))
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


    adresse_koord <- data.frame(adresse, postnummer, lon, lat, kommunenummer, kommunenavn)
    return(adresse_koord_alle <- rbind(adresse_koord_alle, adresse_koord))
  }

  # Vektoriserer funksjon adresse_koord_funk #
  adresse_koord_funk_vec <- Vectorize(adresse_koord_funk, vectorize.args = c("postnummer", "adresse"))

  # Bruker vektorene i funksjonen for å finne koordinatene til hver adresse #
  adresse_koord_alle <- data.frame()
  adresser_koord <- adresse_koord_funk_vec(postnummer, adresse)

  # Gjør om til data frame #
  adresser_koord <- as.data.frame(t(adresser_koord))
  adresser_koord[] <- lapply(adresser_koord, unlist)

  adresser_koord$ID <- rep(1:nrow(adresser_koord))

  # Identifiserer adresser som mangler koordinater #
  adresser_koord_missing <- adresser_koord %>%
    dplyr::filter(is.na(lat))


  if (format == "sf" & nrow(adresser_koord_missing)==0) {
    adresser_koord <- adresser_koord %>%
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4258) %>%
      sf::st_transform(crs = crs)
  }

  if (format == "tibble") {
    adresser_koord <- adresser_koord %>%
      as_tibble()
  }

  return(adresser_koord)

}
