# -*- coding: utf-8 -*-

#' Connect coordinates to the nearest nodes in the road network
#'
#' The function `coords_to_node` can be used to find the nearest nodes in the Norwegian road network (in meters) for chosen coordinates.
#'
#' Before the function can be used, the nodes of the road network must be converted to an `sf` object that is called `nodes` (or another name supplied to the `nodes_object` argument). This can be done with the function [GISSB::vegnett_to_R()].
#'
#' @param coords An `sf` object with the coordinates that should be connected to the road network.
#' @param nodes_object An `sf` object with the nodes of the road network. This can be created with the function [GISSB::vegnett_to_R()].
#' @param edges_object A data frame with the edges of the road network. This can be created with the function [GISSB::vegnett_to_R()].
#' @param direction Character vector with `from` if the points should be from nodes or `to` if the points should be to nodes.
#' @param ID_col Character vector with the name of the ID column. Default value is set to “ID”.
#' @param crs_out Numeric vector for the chosen coordinate reference system (CRS).
#' @param knn Numeric vector with the chosen number of nodes that should be returned for each of the coordinates. If `knn = 1` only the nearest nodes to the chosen coordinates will be returned. If `knn = 2` the two nearest nodes will be returned etc.
#' @param membership Logical. If `TRUE` the search for nodes is limited to nodes that belong to a road network that is connected either to the from or to nodes (only possible for either from or to). E.g. if you only want to search for from nodes that belong to the same road network as the to nodes, membership is set to `FALSE` in the search for to nodes and `membership = TRUE` for the from nodes (in that order).
#'
#'
#' @returns An object (`data.frame`) with the following columns; `from_nodeID`/`to_nodeID`, `membership_from_node`/`membership_to_node`, `coords_google_from_node`/`coords_google_to_node`, `knn_from_node`/`knn_to_node`, and `ID`.
#' @export
#'
#' @examples
#' \dontrun{
#' from <- address_to_coords(zip_code = "0177",
#'                          address = "Akersveien 26")
#' from_node <- coords_to_node(coords = from, direction = "from")
#'
#' to <- address_to_coords(zip_code = "2211",
#'                          address = "Otervegen 23")
#' to_node <- coords_to_node(coords = to, direction = "to")
#' }
#' @encoding UTF-8
#'
#'

coords_to_node <- function(coords,
                           nodes_object = nodes,
                           edges_object = edges,
                           direction = "from",
                           ID_col = "ID",
                           crs_out = 25833,
                           knn = 1,
                           membership = F) {

  nodes <- nodes_object
  edges <- edges_object

  if (direction == "from"){

    nodes_start <- nodes %>%
      dplyr::filter(nodeID %in% unique(edges$from))

    # # OBS
    if (membership == T){
      nodes_start <- nodes_start %>%
        dplyr::filter(membership %in% unique(to_node$membership_to_node))
    }

    coords_start <- nodes_start %>%
      sf::st_coordinates()

    from_coord <- coords %>%
      sf::st_coordinates() %>%
      matrix(ncol = 2)
    colnames(from_coord) <- c("X", "Y")

    node_index_o <- nabor::knn(data = coords_start,
                               query = from_coord,
                               k = knn)

    nodes_start <- sf::st_transform(nodes_start, crs = 4326) %>%
      coords_to_google() %>%
      data.frame() %>%
      dplyr::rename(from_nodeID = nodeID,
                    coords_google_from_node = coords_google,
                    membership_from_node = membership) %>%
      dplyr::select(-geometry)

    start_node <- nodes_start[node_index_o$nn.idx, ]
    start_node$knn_from_node <- rep(1:knn, each=nrow(coords))

    # ID <- coords$ID
    ID <- coords %>%
      dplyr::select(!!as.name(ID_col)) %>%
      data.frame() %>%
      dplyr::select(-geometry)

    dists <-  data.frame(ID, node_index_o$nn.dists)

    dists <- reshape2::melt(dists, id.vars = ID_col,
                            variable.name = "variabel",
                            value.name = "dist_coord_node_from") %>%
      dplyr::select(-variabel)

    start_node <- cbind(start_node, dists)

    return(start_node)

  }

  if (direction == "to"){

    nodes_end <- nodes %>%
      dplyr::filter(nodeID %in% unique(edges$to))

    if (membership == T){
      nodes_end <- nodes_end %>%
        dplyr::filter(membership %in% unique(from_node$membership_from_node))
    }

    coords_end <- nodes_end %>%
      sf::st_coordinates()

    to_coord <- coords %>%
      sf::st_coordinates() %>%
      matrix(ncol = 2)
    colnames(to_coord) <- c("X", "Y")

    node_index_d <- nabor::knn(data = coords_end,
                               query = to_coord,
                               k = 1)

    nodes_end <- sf::st_transform(nodes_end, crs = 4326) %>%
      coords_to_google() %>%
      data.frame() %>%
      dplyr::rename(to_nodeID = nodeID,
                    coords_google_to_node = coords_google,
                    membership_to_node = membership) %>%
      dplyr::select(-geometry)

    end_node <- nodes_end[node_index_d$nn.idx, ]
    end_node$knn_to_node <- rep(1:knn, each=nrow(coords))

    # ID <- coords$ID
    ID <- coords %>%
      dplyr::select(!!as.name(ID_col)) %>%
      data.frame() %>%
      dplyr::select(-geometry)

    dists <-  data.frame(ID, node_index_d$nn.dists)

    # return(dists)

    dists <- reshape2::melt(dists, id.vars = ID_col,
                            variable.name = "variabel",
                            value.name = "dist_coord_node_to") %>%
      dplyr::select(-variabel)

    end_node <- cbind(end_node, dists)

    return(end_node)


  }

}



