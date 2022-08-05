
#' Visualiser kjørerute med Leaflet
#'
#' Funksjonen path_leaflet visualiserer den korteste kjøreruten (i minutter eller meter) som har blitt beregnet med funksjonen beregne_avstand (der path = TRUE).
#' For å få lastet inn bakgrunnskart fra Leaflet kreves nettilgang.
#'
#' @param path Objekt (list) som har blitt laget med funksjonen beregne_avstand (der path = TRUE).
#'
#' @returns Interaktivt kart Leaflet som viser kjøreruten mellom et valgt fra- og tilpunkt på vegnettet.
#' @export
#'
#' @examples
#' path_leaflet(path)
#' @encoding UTF-8
#'
#'


path_leaflet <- function(path) {

  path_graph_length <- graph %>%
    igraph::subgraph.edges(eids = path$epath %>%
                             unlist()) %>%
    tidygraph::as_tbl_graph()

  leaflet_out <- path_graph_length %>%
    tidygraph::activate(edges) %>%
    tibble::as_tibble() %>%
    sf::st_as_sf() %>%
    sf::st_transform(4326) %>%
    leaflet::leaflet() %>%
    leaflet::addPolylines() %>%
    leaflet::addTiles()

  return(leaflet_out)

}




