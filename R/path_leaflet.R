
#' Visualize the shortest path with Leaflet
#'
#' The function `path_leaflet` visualizes the shortest path (in minutes or meters) that has been calculated with the function [GISSB::shortest_path_igraph()] (where path = TRUE).
#' Internet connection is required to load the background tiles.
#'
#' @param path Object (list) that has been created with the function [GISSB::shortest_path_igraph()] (where path = TRUE).
#' @param graph_object The road network structured as a tidy graph (tbl_graph object). This can be done with the function [GISSB::vegnett_to_R()].
#'
#' @returns Interactive Leaflet map that shows the shortest path (in minutes or meters) between a chosen from and to node in the road network.
#' @export
#'
#' @examples
#' \dontrun{
#' path_leaflet(path)
#' }
#' @encoding UTF-8
#'
#'


path_leaflet <- function(path,
                         graph_object = graph) {

  path_graph_length <- graph_object %>%
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




