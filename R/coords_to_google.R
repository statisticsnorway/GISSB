

#' Convert coordinates to Google Maps
#'
#' The function `coords_to_google` can be used to convert coordinates of an sf object to a format that is easy to copy and paste into Google Maps.
#'
#' @param coords An sf object with a geometry column that will be converted to CRS 4326.
#' @param crs_out Chosen coordinate reference system (CRS) for the geometry column of the returned sf object.

#' @returns An sf object with a new column added (`coords_google`), and an addtional sf geometry column with the chosen CRS.
#' @export
#'
#' @examples
#' address_to_coords(zip_code = "0177", address = "Akersveien 26") %>%
#'   coords_to_google()
#' @encoding UTF-8
#'
#'

coords_to_google <- function(coords,
                             crs_out = 25833) {

  # Forenkler koordinatene for å klippe og lime inn i Google Maps #
  coords_4326 <- sf::st_transform(coords, crs = 4326) %>%
    dplyr::mutate(coords_google_1 = gsub("^(.*?),.*", "\\1", as.character(geometry)),
                  coords_google_2 = gsub(".*,", "\\1", as.character(geometry))) %>%
    dplyr::mutate(coords_google_1 = gsub("^c\\(|\\)$", "", as.character(coords_google_1)),
                  coords_google_2 = gsub(").*", "", coords_google_2)) %>%
    dplyr::mutate(coords_google_1 = as.numeric(stringr::str_trim(coords_google_1)),
                  coords_google_2 = as.numeric(stringr::str_trim(coords_google_2))) %>%
    dplyr::mutate(coords_google = paste0(coords_google_2, ", ", coords_google_1)) %>%
    dplyr::select(-coords_google_1, -coords_google_2) %>%
    sf::st_transform(coords, crs = crs_out)
  # data.frame()

  return(coords_4326)

}
