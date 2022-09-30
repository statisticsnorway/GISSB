
#' Shortest path (cppRouting)
#'
#' The function `shortest_path_cppRouting` can be used to calculate the shortest path (either in minutes or meters) between two or more nodes in the Norwegian road network. The function also works with vectors with multiple from and to node ID’s.
#' Before the function can be used, the road network must be converted to a cppRouting object that is called `graph_cppRouting_FT_MINUTES` or `graph_cppRouting_LENGTH`. This can be done with the function [GISSB::vegnett_to_R()].
#'
#' @param from_node_ID Numeric vector with one more from node ID’s.
#' @param to_node_ID Numeric vector with one more to node ID’s.
#' @param unit Character vector with "FT_MINUTES" to calculate the shortest path in minutes or "LENGTH" for the shortest path in meters.
#' @param dist Character vector that specifies if all the shortest paths between all the supplied from and to nodes are returned, or if only the minimum/maximum value per from node ID is returned.
#'
#' @returns Object (data.frame) with how many minutes or meters the shortest path is between the supplied from and to node ID’s.
#' @export
#'
#' @examples
#' \dontrun{
#' distance_min <- shortest_path_cppRouting(26956,
#'                                        210373,
#'                                        unit = "FT_MINUTES")
#'
#' distance_meter <- shortest_path_cppRouting(26956,
#'                                          210373,
#'                                          unit = "LENGTH")
#'                                          }
#' @encoding UTF-8
#'
#'

shortest_path_cppRouting <- function(from_node_ID,
                                to_node_ID,
                                unit = "FT_MINUTES",
                                dist = "all") {

  if (unit == "FT_MINUTES") {
    graph_cppRouting <- graph_cppRouting_FT_MINUTES
  }

  if (unit == "LENGTH") {
    graph_cppRouting <- graph_cppRouting_LENGTH
  }


  dists <- cppRouting::get_distance_matrix(graph_cppRouting,
                                           from = from_node_ID,
                                           to = to_node_ID,
                                           algorithm = "phast")
  dists2 <- data.frame(dists)

  dists2 <- tibble::rownames_to_column(dists2, "from_nodeID")
  dists2_long <- reshape2::melt(dists2, id.vars = "from_nodeID",
                                variable.name = "to_nodeID",
                                value.name = "length")
  dists2_long$to_nodeID <- gsub("X", "", dists2_long$to_nodeID)

  if (dist == "min") {
    dists2_long <- dists2_long %>%
      dplyr::group_by(from_nodeID) %>%
      dplyr::slice(which.min(length))
  }

  if (dist == "max") {
    dists2_long <- dists2_long %>%
      dplyr::group_by(from_nodeID) %>%
      dplyr::slice(which.max(length))
  }

  dists2_long$from_nodeID <- as.integer(dists2_long$from_nodeID)
  dists2_long$to_nodeID <- as.integer(dists2_long$to_nodeID)

  return(dists2_long)

}
