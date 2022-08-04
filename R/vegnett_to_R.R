
#' Konverter vegnett til R
#'
#' Funksjon for å omgjøre vegnettet fra Nasjonal vegdatabank (NVDB) til formater tilpasset nettverksanalyse i R (tbl_graph og cppRouting)
#'
#' @param vegnett Vegnett som et sf-objekt.
#' @param crs_out Numerisk vektor for ønsket koordinatsystem.
#'
#' @returns Liste med følgende objekter:
#'
#' [1] graph: vegnettet strukturert som "tidy graph" (tbl_graph-objekt),
#'
#' [2] nodes: nodene til vegnettet (sf-objekt),
#'
#' [3] edges: veglenkene som vegnettet består av (data.frame),
#'
#' [4] graph_cppRouting_FT_MINUTES: vegnettet strukturert som "cppRouting graph" med kostnaden per veglenke i minutter (cppRouting-objekt)
#'
#' [5] graph_cppRouting_LENGTH: vegnettet strukturert som "cppRouting graph" med kostnaden per veglenke i meter (cppRouting-objekt)
#' @export
#'
#' @examples
#' vegnett_list <- vegnett_to_R(vegnett = vegnett)
#'
#' graph <- vegnett_list[[1]]
#' nodes <- vegnett_list[[2]]
#' edges <- vegnett_list[[3]]
#' graph_cppRouting_FT_MINUTES <- vegnett_list[[4]]
#' graph_cppRouting_LENGTH <- vegnett_list[[5]]
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

  #####################
  ## Databehandling ###
  #####################

  # Legger til en ekstra rad der veien går begge veier #
  # Lager subset med veier som går begge veier (B) og angir retning fra-til (FT) og til-fra (TF) #
  B_FT <- vegnett %>%
    dplyr::filter(ONEWAY == "B") %>%
    dplyr::mutate(direction = "B_FT")

  B_TF <- vegnett %>%
    dplyr::filter(ONEWAY == "B") %>%
    dplyr::mutate(direction = "B_TF")

  # Subset med kun FT #
  FT <- vegnett %>%
    dplyr::filter(ONEWAY == "FT") %>%
    dplyr::mutate(direction = "FT")

  # Subset med kun TF #
  TF <- vegnett %>%
    dplyr::filter(ONEWAY == "TF") %>%
    dplyr::mutate(direction = "TF")

  # Binder sammen alle veglenkene #
  edges <- rbind(B_FT, FT, B_TF, TF) %>%
    dplyr::mutate(edgeID = c(1:n())) %>% # legger til ny edgeID
    dplyr::mutate(FT_MINUTES = case_when( # Setter riktig FT_MINUTES for linjer som går TF
      direction %in% c("B_TF", "TF") ~ TF_MINUTES, TRUE ~ FT_MINUTES))

  # Henter ut nodene til veglenkene og angir start- og end #
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
    dplyr::mutate(xy = paste(.$X, .$Y)) %>% # Legger til nodeID
    dplyr::mutate(xy = factor(xy, levels = unique(xy))) %>%
    dplyr::group_by(xy) %>%
    dplyr::mutate(nodeID = cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-xy, -geometry) #
  # dplyr::select(-xy, -!!as.name(geometry)) # OBS:  Erstatt heller med funksjon tidligere som renamer geometrikolonnen

  # Startnoder #
  source_nodes <- nodes %>%
    dplyr::filter(start_end == 'start') %>%
    dplyr::pull(nodeID)

  # Endnoder #
  target_nodes <- nodes %>%
    dplyr::filter(start_end == 'end') %>%
    dplyr::pull(nodeID)

  # Lager edges fra source_nodes og target_nodes #
  edges <- edges %>%
    dplyr::mutate(from = source_nodes, to = target_nodes)

  # Henter ut distinkte noder med punktkoordinater #
  nodes <- nodes %>%
    dplyr::distinct(nodeID, .keep_all = TRUE) %>%
    dplyr::select(-c(edgeID, start_end)) %>%
    sf::st_as_sf(coords = c('X', 'Y')) %>%
    sf::st_set_crs(sf::st_crs(edges))

  # Lager tbl_graph-objekt av vegnettet #
  graph <- tidygraph::tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = T)

  # Fjerner loops i graph #
  graph <- igraph::simplify(graph, remove.loops = T, remove.multiple = F)
  graph <- tidygraph::as_tbl_graph(graph)

  # Henter ut nye edges (der loops er fjernet) #
  edges <- graph %>%
    tidygraph::activate(edges) %>%
    data.frame()

  membership <- igraph::components(graph)$membership
  membership <- data.frame(membership)

  nodes <- nodes %>%
    cbind(membership) %>%
    dplyr::select(nodeID, geometry, membership) %>%
    sf::st_set_crs(crs_out)


  #############################
  ## Lager cppRouting graph ###
  #############################

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

  ### Lager cppRouting graph ###
  graph_cppRouting_FT_MINUTES <- cppRouting::makegraph(edges_FT_MINUTES, directed = T, coords = node_list_coord)
  graph_cppRouting_LENGTH <- cppRouting::makegraph(edges_LENGTH, directed = T, coords = node_list_coord)


  return(list(graph,
              nodes,
              edges,
              graph_cppRouting_FT_MINUTES,
              graph_cppRouting_LENGTH))

}
