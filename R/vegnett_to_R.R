
#' Convert the Norwegian road network (NVDB Ruteplan nettverksdatasett) into network graphs in R
#'
#' The function `vegnett_to_R` can be used to convert the Norwegian road network, downloaded from \href{https://kartkatalog.geonorge.no/metadata/nvdb-ruteplan-nettverksdatasett/8d0f9066-34f9-4423-be12-8e8523089313/}{Geonorge}, to formats that allows for network analysis in R (tbl_graph and cppRouting).
#'
#' @param vegnett The Norwegian road network, downloaded from \href{https://kartkatalog.geonorge.no/metadata/nvdb-ruteplan-nettverksdatasett/8d0f9066-34f9-4423-be12-8e8523089313/}{Geonorge}, as an sf object.
#' @param crs_out Numeric vector with the chosen coordinate reference system (CRS).
#'
#' @returns List containing the following elements:
#'
#' `[1] graph`: the road network structured as a tidy graph (tbl_graph object)
#'
#' `[2] nodes`: the road network's nodes (sf object)
#'
#' `[3] edges`: road network's edges/node links (data.frame)
#'
#' `[4] graph_cppRouting_FT_MINUTES`: the road network structured as a cppRouting graph with the cost of travel in minutes (cppRouting object)
#'
#' `[5] graph_cppRouting_METERS`: the road network structured as a cppRouting graph with the cost of travel in meters (cppRouting object)
#' @export
#'
#' @examples
#' vegnett_sampledata
#' vegnett_list <- vegnett_to_R(vegnett = vegnett_sampledata)
#'
#' graph <- vegnett_list[[1]]
#' nodes <- vegnett_list[[2]]
#' edges <- vegnett_list[[3]]
#' graph_cppRouting_FT_MINUTES <- vegnett_list[[4]]
#' graph_cppRouting_METERS <- vegnett_list[[5]]
#'
#' graph
#' nodes
#' head(edges)
#' head(graph_cppRouting_FT_MINUTES$data)
#' head(graph_cppRouting_FT_MINUTES$coords)
#' head(graph_cppRouting_FT_MINUTES$dict)
#' graph_cppRouting_FT_MINUTES$nbnode
#'
#' head(graph_cppRouting_METERS$data)
#' head(graph_cppRouting_METERS$coords)
#' head(graph_cppRouting_METERS$dict)
#' graph_cppRouting_METERS$nbnode
#'
#' @encoding UTF-8
#'
#'

vegnett_to_R <- function(vegnett,
                         crs_out = 25833,
                         ferry = TRUE) {

  suppressWarnings(
  vegnett <- vegnett %>%
    sf::st_zm(drop = T) %>%
    dplyr::rename_all(toupper) %>%
    # dplyr::rename(LENGTH = SHAPE_LENGTH) %>% # OBS? Erstatt alle LENGTH med SHAPE_LENGTH
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

  # # OBS?
  # if (is.numeric(ferry)==T){
  # vegnett <- vegnett %>%
  #   dplyr::mutate(km = SHAPE_LENGTH/1000,
  #                 timer = FT_MINUTES/60,
  #                 km_t = km/timer,
  #                 FT_MINUTES_ny = (km/ferry)*60,
  #                 FT_MINUTES = case_when(
  #                   # SPECIALVEG %in% c("[FERGE]", "[FERGE,TURIST]") ~ FT_MINUTES_ny,
  #                   ROADCLASS == 4 ~ FT_MINUTES_ny,
  #                   TRUE ~ FT_MINUTES
  #                 ),
  #                 TF_MINUTES = case_when(
  #                   # SPECIALVEG %in% c("[FERGE]", "[FERGE,TURIST]") ~ FT_MINUTES_ny,
  #                   ROADCLASS == 4 ~ FT_MINUTES_ny,
  #                   TRUE ~ TF_MINUTES
  #                 )) %>%
  #   dplyr::select(-km, -timer, -km_t, FT_MINUTES_ny)
  # }

  ######################
  ## Data processing ###
  ######################

  # Adding an extra row where the road goes both ways #
  # Creating a subset with values where the road goes both ways (B) and specifies direction from-to (FT) and to-from (TF) #
  B_FT <- vegnett %>%
    dplyr::filter(ONEWAY == "B") %>%
    dplyr::mutate(direction = "B_FT") %>%
    dplyr::filter(FT_MINUTES > 0 | TF_MINUTES > 0) # Removes edges where FT_MINUTES or TF_MINUTES is missing


  B_TF <- vegnett %>%
    dplyr::filter(ONEWAY == "B") %>%
    dplyr::mutate(direction = "B_TF") %>%
    dplyr::filter(FT_MINUTES > 0 | TF_MINUTES > 0) # Removes edges where FT_MINUTES or TF_MINUTES is missing


  # Subset with only FT #
  FT <- vegnett %>%
    dplyr::filter(ONEWAY == "FT") %>%
    dplyr::mutate(direction = "FT") %>%
    dplyr::filter(FT_MINUTES > 0) # Removes edges where FT_MINUTES is missing

  # Subset with only TF #
  TF <- vegnett %>%
    dplyr::filter(ONEWAY == "TF") %>%
    dplyr::mutate(direction = "TF") %>%
    dplyr::filter(TF_MINUTES > 0)  # Removes edges where TF_MINUTES is missing

  # Binding together all the edges #
  edges <- rbind(B_FT, FT, B_TF, TF) %>%
    dplyr::mutate(edgeID = c(1:dplyr::n())) %>% # adding new edge ID
    dplyr::mutate(FT_MINUTES = dplyr::case_when( # specify correct FT_MINUTES for edges that go TF
      direction %in% c("B_TF", "TF") ~ TF_MINUTES, TRUE ~ FT_MINUTES))

  # Extracting the nodes from the edges and specifies start and end #
  nodes <- edges %>%
    sf::st_coordinates() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(edgeID = L1) %>%
    dplyr::group_by(edgeID) %>%
    dplyr::slice(c(1, dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(start_end = rep(c('start', 'end'), times = dplyr::n()/2))

  nodes  <- dplyr::left_join(nodes, edges, by = c("edgeID")) %>%
    dplyr::mutate(start_end = dplyr::case_when(
      direction %in% c("B_TF", "TF") & start_end == "start" ~ "end",
      direction %in% c("B_TF", "TF") & start_end == "end" ~ "start", TRUE ~ start_end)) %>%
    dplyr::mutate(xy = paste(.$X, .$Y)) %>% # adding node ID
    dplyr::mutate(xy = factor(xy, levels = unique(xy))) %>%
    dplyr::group_by(xy) %>%
    dplyr::mutate(nodeID = dplyr::cur_group_id()) %>%
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
  graph <- tidygraph::tbl_graph(nodes = nodes, edges = dplyr::as_tibble(edges), directed = T)

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

  # SHAPE_LENGTH #
  edges_SHAPE_LENGTH <- edges %>%
    data.frame() %>%
    dplyr::select(from, to, SHAPE_LENGTH) %>%
    dplyr::rename(weight = SHAPE_LENGTH) %>%
    dplyr::mutate(from = as.character(from),
                  to = as.character(to))

  node_list_coord <- nodes %>%
    dplyr::mutate(X = unlist(purrr::map(geometry,1)),
                  Y = unlist(purrr::map(geometry,2))) %>%
    data.frame() %>%
    dplyr::select(nodeID, X, Y)

  ### Creating cppRouting graph ###
  graph_cppRouting_FT_MINUTES <- cppRouting::makegraph(edges_FT_MINUTES, directed = T, coords = node_list_coord)
  graph_cppRouting_METERS <- cppRouting::makegraph(edges_SHAPE_LENGTH, directed = T, coords = node_list_coord)


  return(list(graph,
              nodes,
              edges,
              graph_cppRouting_FT_MINUTES,
              graph_cppRouting_METERS))

}
