# -*- coding: utf-8 -*-

#' Shortest path (cppRouting)
#'
#' The function `shortest_path_cppRouting` can be used to calculate the shortest path (either in minutes or meters) between two or more nodes in the Norwegian road network. The function also works with vectors with multiple from and to node ID’s.
#' Before the function can be used, the road network must be converted to a cppRouting object that is called `graph_cppRouting_minutes` or `graph_cppRouting_meters`. This can be done with the function [GISSB::vegnett_to_R()].
#'
#' @param from_node_ID Numeric vector with one more from node ID’s.
#' @param to_node_ID Numeric vector with one more to node ID’s.
#' @param unit Character vector with "minutes" to calculate the shortest path in minutes or "meters" for the shortest path in meters.
#' @param dist Character vector that specifies if all the shortest paths between all the supplied from and to nodes are returned, or if only the minimum/maximum value per from node ID is returned.
#' @param graph_cppRouting_object The road network structured as a cppRouting graph object. This can be done with the function [GISSB::vegnett_to_R()].
#'
#' @returns Object (data.frame) with how many minutes or meters the shortest path is between the supplied from and to node ID’s.
#' @export
#'
#' @examples
#' \dontrun{
#' distance_min <- shortest_path_cppRouting(26956,
#'                                        210373,
#'                                        unit = "minutes")
#'
#' distance_meter <- shortest_path_cppRouting(26956,
#'                                          210373,
#'                                          unit = "meters")
#'                                          }
#' @encoding UTF-8
#'
#'

shortest_path_cppRouting <- function(from_node_ID,
                                     to_node_ID,
                                     unit = "minutes",
                                     dist = "all",
                                     graph_cppRouting_object = graph_cppRouting_minutes) {


  graph_cppRouting <- graph_cppRouting_object

  # if (unit == "minutes" & graph_cppRouting_fix == FALSE) {
  #   graph_cppRouting <- graph_cppRouting_minutes
  # }
  #
  # if (unit == "meters" & graph_cppRouting_fix == FALSE) {
  #   graph_cppRouting <- graph_cppRouting_meters
  # }
  #
  # if (graph_cppRouting_fix == TRUE) {
  #   graph_cppRouting <- graph_cppRouting_new
  # }

  # OBS: legg til feilmelding dersom graph_cppRouting_minutes eller graph_cppRouting_meters ikke finnes?


  dists <- cppRouting::get_distance_matrix(graph_cppRouting,
                                           from = from_node_ID,
                                           to = to_node_ID,
                                           algorithm = "phast")
  dists2 <- data.frame(dists)

  dists2 <- tibble::rownames_to_column(dists2, "from_nodeID")
  dists2_long <- reshape2::melt(dists2, id.vars = "from_nodeID",
                                variable.name = "to_nodeID",
                                value.name = unit)
  dists2_long$to_nodeID <- gsub("X", "", dists2_long$to_nodeID)

  if (dist == "min") {
    dists2_long <- dists2_long %>%
      dplyr::group_by(from_nodeID) %>%
      dplyr::slice(which.min(!!sym(unit)))
  }

  if (dist == "max") {
    dists2_long <- dists2_long %>%
      dplyr::group_by(from_nodeID) %>%
      dplyr::slice(which.max(!!sym(unit)))
  }

  dists2_long$from_nodeID <- as.integer(dists2_long$from_nodeID)
  dists2_long$to_nodeID <- as.integer(dists2_long$to_nodeID)

  return(dists2_long)

}
