problem_coords_fix <- function(problem_coords,
                               antall_knn = 1,
                               nodes = nodes,
                               direction = "from",
                               ID_col = "nodeID",
                               extra_length = 1,
                               minutes_per_meter = 0.002) { # 30 km/t: ((1/1000)/30*60)

    from_node <- coords_to_node(coords = problem_coords,
                                nodes = nodes,
                                ID_col = ID_col, # idx
                                knn = antall_knn,
                                direction = "from",
                                membership = FALSE)

    # Legger til ny node for adresse 1 #
    node_list_coord <- nodes %>%
        dplyr::mutate(X = unlist(purrr::map(geometry,1)),
                      Y = unlist(purrr::map(geometry,2))) %>%
        data.frame() %>%
        dplyr::select(nodeID, X, Y)

    ny_node <- problem_coords %>%
        # dplyr::filter(!nodeID %in% from_node$nodeID) %>%
        dplyr::mutate(X = unlist(purrr::map(geometry,1)),
                      Y = unlist(purrr::map(geometry,2))) %>%
        data.frame() %>%
        dplyr::select(nodeID, X, Y)

    node_list_coord <- rbind(node_list_coord, ny_node)

    nodes_new <- node_list_coord %>%
        sf::st_as_sf(coords = c("X", "Y"),
                     crs = 25833) %>%
        dplyr::mutate(membership = 1)

    # Legger til edges for den nye noden #
    edges_minutes <- edges %>%
        data.frame() %>%
        dplyr::select(from, to, minutes, geometry) %>% # OBS: ta med originale edgeID?
        dplyr::rename(weight = minutes) %>%
        dplyr::mutate(from = as.character(from),
                      to = as.character(to))

    ny_edge_from <- from_node %>%
        dplyr::mutate(weight = minutes_per_meter*(dist_coord_node_from*extra_length)) %>% # OBS
        dplyr::select(nodeID, from_nodeID, weight) %>%
        dplyr::rename(from = nodeID,
                      to = from_nodeID) %>%
        dplyr::left_join(nodes_new, by = c("from" = "nodeID")) %>%
        dplyr::left_join(nodes, by = c("to" = "nodeID")) %>%
        sf::st_as_sf()

    ny_edge_from_linestring <- sf::st_sfc(mapply(function(a,b){sf::st_cast(sf::st_union(a,b),"LINESTRING")}, ny_edge_from$geometry.x, ny_edge_from$geometry.y, SIMPLIFY=FALSE))

    ny_edge_from <- cbind(ny_edge_from, ny_edge_from_linestring) %>%
        data.frame() %>%
        dplyr::select(from, to, weight, geometry)

    ny_edge_to <- from_node %>%
        dplyr::mutate(weight = minutes_per_meter*(dist_coord_node_from*extra_length)) %>% # OBS
        dplyr::select(nodeID, from_nodeID, weight) %>%
        dplyr::rename(to = nodeID,
                      from = from_nodeID) %>%
        dplyr::left_join(nodes_new, by = c("to" = "nodeID")) %>%
        dplyr::left_join(nodes, by = c("from" = "nodeID")) %>%
        sf::st_as_sf()

    ny_edge_to_linestring <- sf::st_sfc(mapply(function(a,b){sf::st_cast(sf::st_union(a,b),"LINESTRING")}, ny_edge_to$geometry.x, ny_edge_to$geometry.y, SIMPLIFY=FALSE))

    ny_edge_to <- cbind(ny_edge_to, ny_edge_to_linestring) %>%
        data.frame() %>%
        dplyr::select(from, to, weight, geometry)

    edges_minutes <- rbind(edges_minutes, ny_edge_from, ny_edge_to)

    edges_minutes_wo_geom <- edges_minutes %>%
        dplyr::select(-geometry)

    graph_cppRouting_new <- cppRouting::makegraph(edges_minutes_wo_geom, directed = T, coords = node_list_coord)

    edges_new <- edges_minutes %>%
        dplyr::rename(minutes = weight)

    return(list(graph_cppRouting_new,
                nodes_new,
                edges_new))

    # return(graph_cppRouting_new)
}
