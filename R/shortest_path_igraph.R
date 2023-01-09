# -*- coding: utf-8 -*-


#' Shortest path (igraph)
#'
#' The function `shortest_path_igraph` can be used to calculate the shortest path (either in minutes or meters) between nodes in the Norwegian road network. The function can also return the node link, i.e. path, that the shortest path consists of.
#'
#' Before the function can be used, the road network must be converted to a `tbl_graph` object. This can be done with the function [GISSB::vegnett_to_R()]). There objects `graph` and `edges` needs to be loaded.
#'
#' @param from_node_ID Numeric value with the from node ID (if multiple node ID’s are to be used, see the function [GISSB::shortest_path_cppRouting()]).
#' @param to_node_ID Numeric value with the to node ID (if multiple node ID’s are to be used, see the function [GISSB::shortest_path_cppRouting()]).
#' @param graph_object The road network structured as a tidy graph (`tbl_graph` object). This can be created with the function [GISSB::vegnett_to_R()].
#' @param unit Character vector with `minutes` to calculate the shortest path in minutes or `meters` for the shortest path in meters.
#' @param path Logical. If `TRUE` the node link with the shortest path is returned.
#'
#' @returns Vector with the shortest path in minutes or meters. If `path = TRUE` the node link that the shortest path consists of is returned.
#' @export
#'
#' @examples
#' shortest_path_igraph(from_node_ID = 25,
#'                      to_node_ID = 33,
#'                      unit = "minutes",
#'                      graph_object = graph_sampledata)
#'
#' shortest_path_igraph(from_node_ID = 25,
#'                      to_node_ID = 33,
#'                      unit = "meters",
#'                      graph_object = graph_sampledata)
#'
#' shortest_path_igraph(from_node_ID = 25,
#'                      to_node_ID = 33,
#'                      unit = "minutes",
#'                      path = TRUE,
#'                      graph_object = graph_sampledata)
#'
#' @encoding UTF-8
#'
#'

shortest_path_igraph <- function(from_node_ID,
                                 to_node_ID,
                                 graph_object = graph,
                                 unit = "minutes",
                                 path = FALSE) {

  path_graph <- igraph::shortest_paths(
    graph = graph_object,
    from = from_node_ID,
    to = to_node_ID,
    output = 'both',
    weights = graph_object %>% tidygraph::activate(edges) %>% dplyr::pull(!!as.name(unit))

  )

  path_graph_length <- graph_object %>%
    igraph::subgraph.edges(eids = path_graph$epath %>%
                             unlist()) %>%
    tidygraph::as_tbl_graph()

  distance <- path_graph_length %>%
    tidygraph::activate(edges) %>%
    tibble::as_tibble() %>%
    dplyr::summarise(length = sum(!!as.name(unit)))


  if (path == TRUE) {
    return(path_graph)
  } else {
    return(distance)
  }

}
