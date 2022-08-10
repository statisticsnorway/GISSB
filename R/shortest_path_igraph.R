

#' Shortest path (igraph)
#'
#' Function to calculate the shortest path (either in minutes or meters) between nodes in the Norwegian road network. The function can also return the node link (path) that the shortest path consists of.
#'
#' Before the function can be used, the road network must be converted to a tbl_graph object that is called “graph”. This can be done with the function [GISSB::vegnett_to_R()]).
#'
#' @param from_node_ID Numeric value with the from node ID (if multiple node ID’s are to be used, see the function [GISSB::shortest_path_cppRouting()]).
#' @param to_node_ID Numeric value with the to node ID (if multiple node ID’s are to be used, see the function [GISSB::shortest_path_cppRouting()]).
#' @param unit Character vector with "FT_MINUTES" to calculate the shortest path in minutes or "LENGTH" for the shortest path in meters.
#' @param path Logical. If TRUE the node link with the shortest path is returned.
#'
#' @returns Vector with the shortest path in minutes or meters. If path = TRUE the node link that the shortest path consists of is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' distance_min <- shortest_path_igraph(from_node_ID = 26956,
#'                                to_node_ID = 210373,
#'                                unit = "FT_MINUTES")
#'
#' distance_meter <- shortest_path_igraph(from_node_ID = 26956,
#'                                  to_node_ID = 210373,
#'                                  unit = "LENGTH")
#'
#' path <- shortest_path_igraph(from_node_ID = 26956,
#'                         to_node_ID = 210373,
#'                         unit = "FT_MINUTES",
#'                         path = T)
#'                         }
#' @encoding UTF-8
#'
#'

shortest_path_igraph <- function(from_node_ID,
                            to_node_ID,
                            unit = "FT_MINUTES",
                            path = F) {

  path_graph <- igraph::shortest_paths(
    graph = graph,
    from = from_node_ID,
    to = to_node_ID,
    output = 'both',
    weights = graph %>% tidygraph::activate(edges) %>% pull(!!as.name(unit))

  )

  path_graph_length <- graph %>%
    igraph::subgraph.edges(eids = path_graph$epath %>%
                             unlist()) %>%
    tidygraph::as_tbl_graph()

  distance <- path_graph_length %>%
    tidygraph::activate(edges) %>%
    tibble::as_tibble() %>%
    dplyr::summarise(length = sum(!!as.name(unit)))


  if (path == T) {
    return(path_graph)
  } else {
    return(distance)
  }

}
