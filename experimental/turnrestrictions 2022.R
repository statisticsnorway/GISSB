
# devtools::install_github("statisticsnorway/GISSB", auth_token = getPass::getPass("PAT: "))

#########################
### Turn restrictions ###
#########################

start.time <- Sys.time()

aargang <- 2022

library(sf)
library(tidyverse)
library(sfarrow)
library(leaflet)
library(tidygraph)
library(cppRouting)

# Laster inn vegnettet #
sf::st_layers("C:/Users/rdn/Documents/Kart/Vegnett/vegnettRuteplan_FGDB_20220703.gdb")
turnrestrictions_geom <- sf::read_sf("C:/Users/rdn/Documents/Kart/Vegnett/vegnettRuteplan_FGDB_20220703.gdb", layer = "turnrestrictions_geom")
turnrestrictions_geom <- turnrestrictions_geom %>%
  dplyr::filter(!(fromFromNode == "1450893" & fromToNode == "1499638" & toToNode == "1473826"))
  # dplyr::filter((fromFromNode == "1473826" & fromToNode == "1499638" & toToNode == "1450893") |
  #                 (fromFromNode == "1450893" & fromToNode == "1499638" & toToNode == "1473826"))


# Lagrer som .parquet
# arrow::write_parquet(turnrestrictions_geom, "C:/Users/rdn/Documents/Kart/Vegnett/turnrestrictions_geom_2022.parquet")

vegnett_parquet_filsti <- paste0("C:/Users/rdn/Documents/Kart/Vegnett/vegnett", aargang, ".parquet")

ds <- arrow::open_dataset(vegnett_parquet_filsti)

vegnett <- ds %>%
  # dplyr::filter(fromnode %in% c("1473826", "1499638", "1450893") |
  #               tonode %in% c("1473826", "1499638", "1450893")) %>%
  dplyr::filter(municipality == "301") %>%
  sfarrow::read_sf_dataset() %>%
  dplyr::rename_all(toupper) %>%
  sf::st_zm(drop = T) %>%
  sf::st_cast("LINESTRING") %>%
  dplyr::rename(FT_MINUTES = DRIVETIME_FW,
                TF_MINUTES = DRIVETIME_BW) %>%
  dplyr::filter(ONEWAY == "TF" & TF_MINUTES > 0 |
                  ONEWAY == "FT" & FT_MINUTES > 0 |
                  ONEWAY == "B" & FT_MINUTES > 0 |
                  ONEWAY == "B" & TF_MINUTES > 0)

# source: 1473826 - target: 1499638
# source 1499638 - target 1450893

rename_geometry <- function(g, name){
  current = attr(g, "sf_column")
  names(g)[names(g)==current] = name
  sf::st_geometry(g)=name
  g
}

vegnett <- rename_geometry(vegnett, "geometry")
sf::st_geometry(vegnett) <- "geometry"

vegnett <- vegnett %>%
sf::st_transform(crs = 4326)

# OBS
# vegnett <- vegnett %>%
  # data.frame() %>%
  # dplyr::filter(FROMNODE == "1499638" | TONODE == "1499638") # %>%
  # dplyr::select(FROMNODE, TONODE, ONEWAY, FT_MINUTES, TF_MINUTES, LINKID)


# OBS <- vegnett %>%
#   # dplyr::filter(FT_MINUTES < 0) %>%
#   dplyr::select(FT_MINUTES, TF_MINUTES, ONEWAY, SHAPE_LENGTH) %>%


###

# ggplot() +
#   geom_sf(data = vegnett,  aes(geometry = geometry))


# vegnett %>%
#   leaflet::leaflet() %>%
#   leaflet::addPolylines() %>%
#   leaflet::addTiles()

###########################################################################################################################

crs_out <- 4326

######################
## Data processing ###
######################

# Adding an extra row where the road goes both ways #
# Creating a subset with values where the road goes both ways (B) and specifies direction from-to (FT) and to-from (TF) #
B_FT <- vegnett %>%
  dplyr::filter(ONEWAY == "B") %>%
  dplyr::mutate(direction = "B_FT") # %>%
  # dplyr::filter(!LINKID %in% unique(turnrestrictions_geom$fromLinkID))

# B_FT$LINKID
# B_FT %>%
#   dplyr::filter(LINKID %in% unique(turnrestrictions_geom$toLinkID)) %>%
#   data.frame() %>%
#   dplyr::select(LINKID, FROMNODE, TONODE, ONEWAY, direction)

# 75379 # = 1473826
# 79205 = 1499638
# 60937 = 1450893


B_TF <- vegnett %>%
  dplyr::filter(ONEWAY == "B") %>%
  dplyr::mutate(direction = "B_TF") # %>%
  # dplyr::filter(!LINKID %in% unique(turnrestrictions_geom$toLinkID))


# Subset with only FT #
FT <- vegnett %>%
  dplyr::filter(ONEWAY == "FT") %>%
  dplyr::mutate(direction = "FT") # %>%
  # dplyr::filter(!LINKID %in% unique(turnrestrictions_geom$fromLinkID))

# Subset with only TF #
TF <- vegnett %>%
  dplyr::filter(ONEWAY == "TF") %>%
  dplyr::mutate(direction = "TF") # %>%
  # dplyr::filter(!LINKID %in% unique(turnrestrictions_geom$toLinkID))

# Binding together all the edges #
edges <- rbind(B_FT, FT, B_TF, TF) %>%
  dplyr::mutate(edgeID = c(1:dplyr::n())) %>% # adding new edge ID
  dplyr::mutate(FT_MINUTES = dplyr::case_when( # specify correct FT_MINUTES for edges that go TF
    direction %in% c("B_TF", "TF") ~ TF_MINUTES, TRUE ~ FT_MINUTES),
    FROMNODEID = case_when(
      direction %in% c("B_TF", "TF") ~ TONODE,
      TRUE ~ FROMNODE),
    TONODEID = case_when(
      direction %in% c("B_TF", "TF") ~ FROMNODE, # OBS: TF/FT?
      TRUE ~ TONODE)
    )

# edges_3 <- dplyr::left_join(turnrestrictions_geom, edges, by = c("FROMNODEID", "TONODEID")) %>%
#   dplyr::filter(!is.na(LINKID))

turnrestrictions_geom <- turnrestrictions_geom %>%
  dplyr::rename(FROMNODEID = fromToNode,
                TONODEID = toToNode) %>%
  dplyr::mutate(turn = 1)

edges <- dplyr::left_join(edges, turnrestrictions_geom, by = c("FROMNODEID", "TONODEID")) %>%
  dplyr::filter(is.na(turn))

# edges <- edges %>%
#   dplyr::filter(is.na(turn)) # %>%
  # dplyr::select(FROMNODEID, TONODEID, LINKID, direction, turn)

# nrow(edges_2)-nrow(edges)

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

# # OBS?
# nodes <- nodes %>%
#   dplyr::filter(!is.na(direction))

# OBS!
# nodes <- nodes %>%
#   dplyr::mutate(nodeID = case_when(
#     direction %in% c("B_FT", "FT") ~ FROMNODEID,
#     direction %in% c("B_TF", "TF") ~ TONODEID,
#     TRUE ~ ""
#   ))

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
  dplyr::mutate(from = source_nodes, to = target_nodes) # %>%
  # dplyr::filter(is.na(turn))


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
graph_cppRouting_SHAPE_LENGTH <- cppRouting::makegraph(edges_SHAPE_LENGTH, directed = T, coords = node_list_coord)



###########################################################################################################################

# from <- 75379 # 75379
# to <- 80855 # 60937

# Når man står i fromFromNode

turnrestrictions_geom_2 <- turnrestrictions_geom %>%
  dplyr::filter(fromLinkID %in% unique(edges$LINKID) |
                  toLinkID %in% unique(edges$LINKID)) # %>%
  # dplyr::filter(fromToNode == "1499638")


from_node <- edges %>%
  dplyr::filter(FROMNODEID == "1473826") %>%
  data.frame() %>%
  dplyr::select(FROMNODEID, from)
from_node <- unique(from_node$from)


# 75379 # = 1473826
# 79205 = 1499638
# 6093760937 = 1450893

# from <- 1473826
# to <- 1450893

# OBS: nå fungerer det for dette ene eksempeelet, men logikken er vel litt motsatt?
# from <- 75376
# to <- 60935

# from_node_original <- 1473826
# to_node_original <- 1450893

from_node_original <- 1450893
to_node_original <- 1473826

from_node <- edges %>%
  dplyr::filter(FROMNODEID == from_node_original) %>%
  data.frame() %>%
  dplyr::select(FROMNODEID, from)

to_node <- edges %>%
  dplyr::filter(TONODEID == to_node_original) %>%
  data.frame() %>%
  dplyr::select(TONODEID, to)

GISSB::shortest_path_igraph(from_node_ID = unique(from_node$from),
                            to_node_ID = unique(to_node$to),
                            unit = "FT_MINUTES")

path <- GISSB::shortest_path_igraph(from_node_ID = unique(from_node$from),
                               to_node_ID = unique(to_node$to),
                               unit = "FT_MINUTES",
                               path = T)

GISSB::path_leaflet(path)


###########################################################################################################################

node_points <- nodes %>%
  dplyr::mutate(long = unlist(map(nodes$geometry,1)),
         lat = unlist(map(nodes$geometry,2)))

node_points %>%
  dplyr::filter(nodeID %in% c(from, to)) %>% # 79205
  # dplyr::filter(nodeID %in% c(75379, 60937)) %>%
  # dplyr::filter(nodeID %in%c(4, 6, 1)) %>% # 1473826, 1499638, 1450893
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addMarkers(~long, ~lat, popup = ~as.character(paste(nodeID, ": ", lat, ", ", long)))



# path_graph_length <- graph %>%
#   igraph::subgraph.edges(eids = path$epath %>%
#                            unlist()) %>%
#   tidygraph::as_tbl_graph()
#
# leaflet_out <- path_graph_length %>%
#   tidygraph::activate(edges) %>%
#   tibble::as_tibble() %>%
#   sf::st_as_sf() %>%
#   sf::st_transform(4326) %>%
#   leaflet::leaflet() %>%
#   leaflet::addPolylines() %>%
#   leaflet::addTiles()
#
# leaflet_out




end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
