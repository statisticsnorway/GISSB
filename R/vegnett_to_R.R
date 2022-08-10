
#' Convert the road network from NVDB into network graphs in R
#'
#' Function to convert the Norwegian road network, downloaded from Nasjonal vegdatabank (NVDB), to formats that allows for network analysis in R (tbl_graph and cppRouting).
#'
#' @param vegnett The Norwegian road network, downloaded from Nasjonal vegdatabank (NVDB), as an sf object.
#' @param crs_out Numeric vector with the chosen coordinate reference system (CRS).
#'
#' @returns List containing the following elements:
#'
#' [1] `graph`: the road network structured as a tidy graph (tbl_graph object)
#'
#' [2] `nodes`: the road network's nodes (sf object)
#'
#' [3] `edges`: road network's edges/node links (data.frame)
#'
#' [4] `graph_cppRouting_FT_MINUTES`: the road network structured as a cppRouting graph with the cost of travel in minutes (cppRouting object)
#'
#' [5] `graph_cppRouting_LENGTH`: the road network structured as a cppRouting graph with the cost of travel in meters (cppRouting object)
#' @export
#'
#' @examples
#' \dontrun{
#' vegnett_list <- vegnett_to_R(vegnett = vegnett)
#'
#' graph <- vegnett_list[[1]]
#' nodes <- vegnett_list[[2]]
#' edges <- vegnett_list[[3]]
#' graph_cppRouting_FT_MINUTES <- vegnett_list[[4]]
#' graph_cppRouting_LENGTH <- vegnett_list[[5]]
#' }
#' @encoding UTF-8
#'
#'

vegnett_to_R <- function(vegnett,
                         crs_out = 25833) {

  suppressWarnings(
  vegnett <- vegnett %>%
    sf::st_zm(drop = T) %>%
    dplyr::rename_all(toupper) %>%
    dplyr::rename(LENGTH = SHAPE_LENGTH) %>%
    sf::st_cast("LINESTRING")
  )

  rename_geometry <- function(g, name){
    current = attr(g, "sf_column")
    names(g)[names(g)==current] = name
    sf::st_geometry(g)=name
    g
  }

  vegnett <- rename_geometry(vegnett, "geometry")

  sf::st_geometry(vegnett) <- "geometry"

  ######################
  ## Data processing ###
  ######################

  # Adding an extra row where the road goes both ways #
  # Creating a subset with values where the road goes both ways (B) and specifies direction from-to (FT) and to-from (TF) #
  B_FT <- vegnett %>%
    dplyr::filter(ONEWAY == "B") %>%
    dplyr::mutate(direction = "B_FT")

  B_TF <- vegnett %>%
    dplyr::filter(ONEWAY == "B") %>%
    dplyr::mutate(direction = "B_TF")

  # Subset with only FT #
  FT <- vegnett %>%
    dplyr::filter(ONEWAY == "FT") %>%
    dplyr::mutate(direction = "FT")

  # Subset with only TF #
  TF <- vegnett %>%
    dplyr::filter(ONEWAY == "TF") %>%
    dplyr::mutate(direction = "TF")

  # Binding together all the edges #
  edges <- rbind(B_FT, FT, B_TF, TF) %>%
    dplyr::mutate(edgeID = c(1:n())) %>% # adding new edge ID
    dplyr::mutate(FT_MINUTES = case_when( # specify correct FT_MINUTES for edges that go TF
      direction %in% c("B_TF", "TF") ~ TF_MINUTES, TRUE ~ FT_MINUTES))

  # Extracting the nodes from the edges and specifies start and end #
  nodes <- edges %>%
    sf::st_coordinates() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(edgeID = L1) %>%
    dplyr::group_by(edgeID) %>%
    dplyr::slice(c(1, n())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(start_end = rep(c('start', 'end'), times = n()/2))

  nodes  <- dplyr::left_join(nodes, edges, by = c("edgeID")) %>%
    dplyr::mutate(start_end = case_when(
      direction %in% c("B_TF", "TF") & start_end == "start" ~ "end",
      direction %in% c("B_TF", "TF") & start_end == "end" ~ "start", TRUE ~ start_end)) %>%
    dplyr::mutate(xy = paste(.$X, .$Y)) %>% # adding node ID
    dplyr::mutate(xy = factor(xy, levels = unique(xy))) %>%
    dplyr::group_by(xy) %>%
    dplyr::mutate(nodeID = cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-xy, -geometry) #

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

  # Extracting distinct nodes with coordinates #
  nodes <- nodes %>%
    dplyr::distinct(nodeID, .keep_all = TRUE) %>%
    dplyr::select(-c(edgeID, start_end)) %>%
    sf::st_as_sf(coords = c('X', 'Y')) %>%
    sf::st_set_crs(sf::st_crs(edges))

  # Creating tbl_graph object of the road network #
  graph <- tidygraph::tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = T)

  # Removing loops in the graph #
  graph <- igraph::simplify(graph, remove.loops = T, remove.multiple = F)
  graph <- tidygraph::as_tbl_graph(graph)

  # Extracting new edges (where loops are removed) #
  edges <- graph %>%
    tidygraph::activate(edges) %>%
    data.frame()

  membership <- igraph::components(graph)$membership
  membership <- data.frame(membership)

  nodes <- nodes %>%
    cbind(membership) %>%
    dplyr::select(nodeID, geometry, membership) %>%
    sf::st_set_crs(crs_out)


  ################################
  ## Creating cppRouting graph ###
  ################################

  # FT_MINUTES #
  edges_FT_MINUTES <- edges %>%
    data.frame() %>%
    dplyr::select(from, to, FT_MINUTES) %>%
    dplyr::rename(weight = FT_MINUTES) %>%
    dplyr::mutate(from = as.character(from),
                  to = as.character(to))

  # LENGTH #
  edges_LENGTH <- edges %>%
    data.frame() %>%
    dplyr::select(from, to, LENGTH) %>%
    dplyr::rename(weight = LENGTH) %>%
    dplyr::mutate(from = as.character(from),
                  to = as.character(to))

  node_list_coord <- nodes %>%
    dplyr::mutate(X = unlist(purrr::map(geometry,1)),
                  Y = unlist(purrr::map(geometry,2))) %>%
    data.frame() %>%
    dplyr::select(nodeID, X, Y)

  ### Creating cppRouting graph ###
  graph_cppRouting_FT_MINUTES <- cppRouting::makegraph(edges_FT_MINUTES, directed = T, coords = node_list_coord)
  graph_cppRouting_LENGTH <- cppRouting::makegraph(edges_LENGTH, directed = T, coords = node_list_coord)


  return(list(graph,
              nodes,
              edges,
              graph_cppRouting_FT_MINUTES,
              graph_cppRouting_LENGTH))

}
