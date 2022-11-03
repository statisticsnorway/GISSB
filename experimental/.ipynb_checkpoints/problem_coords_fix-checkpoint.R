problem_coords_fix <- function(problem_coords, 
                              antall_knn = 1, 
                              nodes = nodes,
                               direction = "from",
                               ID_col = "nodeID", 
                               dist_coord = "", 
                               minutes_per_meter = 0.0021980747194065) { # ?

from_node <- coords_to_node(coords = problem_coords, 
                            nodes = nodes,
                            ID_col = "nodeID", # idx
                            knn = antall_knn,
                            direction = "from", 
                            membership = FALSE)
    
if (is.numeric(dist_coord)) {    
from_node <- from_node %>%
    dplyr::filter(dist_coord_node_from < dist_coord)
    }

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

# Legger til edges for den nye noden #
edges_minutes <- edges %>%
    data.frame() %>%
    dplyr::select(from, to, minutes) %>%
    dplyr::rename(weight = minutes) %>%
    dplyr::mutate(from = as.character(from),
                  to = as.character(to))

ny_edge_from <- from_node %>%
dplyr::mutate(weight = minutes_per_meter*dist_coord_node_from) %>% # OBS: er dette tallet riktig?
dplyr::select(nodeID, from_nodeID, weight) %>%
dplyr::rename(from = nodeID, 
             to = from_nodeID)

ny_edge_to <- from_node %>%
dplyr::mutate(weight = minutes_per_meter*dist_coord_node_from) %>% # OBS: er dette tallet riktig?
dplyr::select(nodeID, from_nodeID, weight) %>%
dplyr::rename(to = nodeID, 
             from = from_nodeID) # %>%
# dplyr::mutate(weight = 0)

edges_minutes <- rbind(edges_minutes, ny_edge_from, ny_edge_to)

graph_cppRouting_new <- cppRouting::makegraph(edges_minutes, directed = T, coords = node_list_coord)
    
return(list(graph_cppRouting_new, 
           node_list_coord, 
           edges_minutes))

    # return(graph_cppRouting_new)
}