

#' Koordinater til Google Maps
#'
#' Funksjon for å omgjøre koordinater til et format som er enkelt å klippe og lime inn i Google Maps.
#'
#' @param koords sf-objekt med en geometrikolonne som skal omgjøres til CRS 4326
#' @param crs_out Ønsket koordinatsystem (CRS) for geometrikolonnen til sf-objektet som returneres

#' @returns sf-objekt med en ny kolonne lagt til (coords_google)
#' @export
#'
#' @examples
#' fra <- adresse_api_koord(postnummer = "0177",
#'                          adresse = "Akersveien 26") %>%
#'   koords_to_google()
#'
#' @encoding UTF-8
#'
#'

koords_to_google <- function(koords,
                             crs_out = 25833) {

  # Forenkler koordinatene for å klippe og lime inn i Google Maps #
  koords_4326 <- sf::st_transform(koords, crs = 4326) %>%
    dplyr::mutate(coords_google_1 = gsub("^(.*?),.*", "\\1", as.character(geometry)),
                  coords_google_2 = gsub(".*,", "\\1", as.character(geometry))) %>%
    dplyr::mutate(coords_google_1 = gsub("^c\\(|\\)$", "", as.character(coords_google_1)),
                  coords_google_2 = gsub(").*", "", coords_google_2)) %>%
    dplyr::mutate(coords_google_1 = as.numeric(str_trim(coords_google_1)),
                  coords_google_2 = as.numeric(str_trim(coords_google_2))) %>%
    dplyr::mutate(coords_google = paste0(coords_google_2, ", ", coords_google_1)) %>%
    dplyr::select(-coords_google_1, -coords_google_2) %>%
    sf::st_transform(koords, crs = crs_out)
  # data.frame()

  return(koords_4326)

}
