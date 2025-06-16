
coords_to_node_2 <- function(coords,
                           nodes_object = nodes,
                           edges_object = edges,
                           direction = "from",
                           ID_col = "ID",
                           crs_out = 25833,
                           knn = 1,
                           membership = FALSE) {
  
  nodes <- nodes_object
  edges <- edges_object
  
  if (direction == "from"){
    
    nodes_start <- nodes %>%
      dplyr::filter(nodeID %in% unique(edges$from))
    
    # # OBS
    if (membership == TRUE){
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
    
    if (membership == TRUE){
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
    
    if (direction == "both"){
      # nodes_end <- nodes %>%
      #   dplyr::filter(nodeID %in% unique(edges$to))
      
      if (membership == TRUE){
        nodes <- nodes %>%
          dplyr::filter(membership %in% unique(from_node$membership_from_node))
      }
      
      both_coords <- nodes %>%
        sf::st_coordinates()
      
      to_coord <- coords %>%
        sf::st_coordinates() %>%
        matrix(ncol = 2)
      colnames(to_coord) <- c("X", "Y")
      
      node_index_d <- nabor::knn(data = both_coords,
                                 query = to_coord,
                                 k = 1)
      
      nodes <- sf::st_transform(nodes, crs = 4326) %>%
        coords_to_google() %>%
        data.frame() %>%
        dplyr::rename(nodeID = nodeID,
                      coords_google_to_node = coords_google,
                      membership_to_node = membership) %>%
        dplyr::select(-geometry)
      
      node <- nodes[node_index_d$nn.idx, ]
      node$knn_to_node <- rep(1:knn, each=nrow(coords))
      
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
      
      node <- cbind(node, dists)
      
      return(node)
    }

}
