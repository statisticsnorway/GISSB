sykkelvegnett_to_R <- function(sykkelvegnett,
                               crs_out = 25833,
                               snap_knn = 50,
                               snap_meters = 30) {


  sykkelvegnett <- sykkelvegnett  %>%
    sf::st_zm(drop = TRUE) %>%
    dplyr::filter(sf::st_geometry_type(geometry) != "POINT") %>%
    sf::st_cast("LINESTRING") %>%
    dplyr::mutate(meters = as.numeric(sf::st_length(geometry))) # adding meters from length of edges

  edges <- sykkelvegnett %>%
    dplyr::mutate(edgeID = c(1:dplyr::n())) # %>% # adding new edge ID

  # Extracting the nodes from the edges and specifies start and end #
  nodes <- edges %>%
    sf::st_coordinates() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(edgeID = L1) %>%
    dplyr::group_by(edgeID) %>%
    dplyr::slice(c(1, dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(start_end = rep(c('start', 'end'), times = dplyr::n()/2))

  # Adding new edges between nodes that are located X meters (see `snap_meters`) apart. Looking among the X nearest neighbors (see `snap_knn`).
  if (!is.null(snap_knn)){

    # Locating the nearest neighbors for each node (knn > 2)
    nodes_problem <- nodes %>%
      dplyr::mutate(row_number = 1:n())

    coords <- nodes_problem %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = crs_out) %>%
      sf::st_coordinates() %>%
      matrix(ncol = 2)
    colnames(coords) <- c("X", "Y")

    node_index <- suppressWarnings(nabor::knn(data = coords,
                                              query = nodes_problem,
                                              k = snap_knn))


    nodes_bonus_all <- data.frame()
    for (i in c(2:snap_knn)){
      dists <- node_index$nn.dists[,i]
      id <- node_index$nn.idx[,i]
      closest <- data.frame(dists, id)

      closest_to <- closest %>%
        dplyr::left_join(nodes_problem, by = c("id" = "row_number")) %>%
        dplyr::rename(edgeID_to = edgeID,
                      start_end_to = start_end,
                      X_to = X,
                      Y_to = Y)

      if (i == 2){
        start_id <- max(edges$edgeID) + (i-1)
      } else {
        start_id <- max(nodes_bonus_all$edgeID) + (i-1)
      }

      nodes_bonus <- data.frame(cbind(nodes_problem, closest_to)) %>%
        dplyr::filter(edgeID != edgeID_to) %>%
        dplyr::filter(dists < snap_meters) %>%
        dplyr::mutate(edgeID = dplyr::row_number()+start_id) %>%
        mutate(snap_knn = i)

      nodes_bonus_all <- rbind(nodes_bonus_all, nodes_bonus)
    }

    # Removing duplicates for bonus nodes
    rows_to_remove <- nodes_bonus_all %>%
      dplyr::mutate(pair_id = paste0(X, Y, X_to, Y_to),
                    pair_id_2 = paste0(X_to, Y_to, X, Y),
                    row_id = row_number()) %>%
      dplyr::select(pair_id, pair_id_2, row_id) %>%
      dplyr::mutate(pair_identifier = pmin(pair_id, pair_id_2),
                    pair_identifier = pmax(pair_id, pair_id_2)) %>%
      dplyr::filter(duplicated(pair_identifier)) %>%
      dplyr::pull(row_id)

    nodes_bonus_all <- nodes_bonus_all[-c(rows_to_remove),] %>%
      dplyr::filter(dists > 0)

    # Identifying start nodes
    nodes_bonus_start <- nodes_bonus_all %>%
      select(edgeID, X, Y) %>%
      mutate(start_end = "start")

    # Identifying end nodes
    nodes_bonus_end <- nodes_bonus_all %>%
      dplyr::select(edgeID, X_to, Y_to) %>%
      dplyr::rename( X = X_to,
                     Y = Y_to) %>%
      dplyr::mutate(start_end = "end")

    # Combining start and end nodes
    nodes_bonus_final <- rbind(nodes_bonus_start, nodes_bonus_end) %>%
      dplyr::arrange(edgeID)

    # Converting nodes_bonus_final to sf object
    nodes_bonus_sf <- nodes_bonus_final %>%
      sf::st_as_sf(coords = c('X', 'Y')) %>%
      sf::st_set_crs(crs_out)

    # Creating bonus edges
    edges_bonus <- nodes_bonus_sf %>%
      dplyr::group_by(edgeID) %>%
      dplyr::summarise(do_union = FALSE) %>%
      sf::st_cast("LINESTRING") %>%
      dplyr::mutate(meters = as.numeric(sf::st_length(geometry)))

    # Adding bonus nodes and edges to the main nodes and edges objects
    nodes <- rbind(nodes, nodes_bonus_final)
    edges <- dplyr::bind_rows(edges, edges_bonus)

  }

  nodes  <- dplyr::left_join(nodes, edges, by = c("edgeID")) %>%
    dplyr::mutate(xy = paste(.$X, .$Y)) %>% # adding node ID
    dplyr::mutate(xy = factor(xy, levels = unique(xy))) %>%
    dplyr::group_by(xy) %>%
    dplyr::mutate(nodeID = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-xy, -geometry)

  # Start nodes #
  source_nodes <- nodes %>%
    dplyr::filter(start_end == 'start') %>%
    dplyr::pull(nodeID)

  # End nodes #
  target_nodes <- nodes %>%
    dplyr::filter(start_end == 'end') %>%
    dplyr::pull(nodeID)

  # Creating edges from source_nodes and target_nodes #
  edges <- edges %>%
    dplyr::mutate(from = source_nodes, to = target_nodes)

  nodes <- nodes %>%
    dplyr::distinct(nodeID, .keep_all = TRUE) %>%
    sf::st_as_sf(coords = c('X', 'Y')) %>%
    sf::st_set_crs(sf::st_crs(edges))

  # Creating tbl_graph object of the road network #
  graph <- tidygraph::tbl_graph(nodes = nodes, edges = dplyr::as_tibble(edges), directed = FALSE)

  # Removing loops in the graph #
  graph <- igraph::simplify(graph, remove.loops = TRUE, remove.multiple = FALSE)
  graph <- tidygraph::as_tbl_graph(graph)

  edges <- graph %>%
    tidygraph::activate(edges) %>%
    data.frame()

  membership <- igraph::components(graph)$membership
  membership <- data.frame(membership)

  nodes <- nodes %>%
    cbind(membership) %>%
    dplyr::select(nodeID, geometry, membership) %>%
    dplyr::filter(nodeID %in% unique(edges$from) | nodeID %in% unique(edges$to)) %>%
    sf::st_set_crs(crs_out)


  ################################
  ## Creating cppRouting graph ###
  ################################

  edges_meters <- edges %>%
    data.frame() %>%
    dplyr::select(from, to, meters) %>%
    dplyr::rename(weight = meters) %>%
    dplyr::mutate(from = as.character(from),
                  to = as.character(to))

  node_list_coord <- nodes %>%
    dplyr::mutate(X = unlist(purrr::map(geometry,1)),
                  Y = unlist(purrr::map(geometry,2))) %>%
    data.frame() %>%
    dplyr::select(nodeID, X, Y)

  graph_cppRouting_meters <- cppRouting::makegraph(edges_meters, directed = FALSE, coords = node_list_coord)

  return(list(graph,
              nodes,
              edges,
              graph_cppRouting_meters))

}
