

# devtools::install_github("statisticsnorway/GISSB", auth_token = getPass::getPass("PAT: "))

#########################
### Turn restrictions ###
#########################

start.time <- Sys.time()

aargang <- 2021
# OBS: FROMNODEID/FROMNODE (lag dynamisk variabel?)
# OBS: TURNRESTRICTIONS (lag argument?)

crs_out <- 4326

library(sf)
library(tidyverse)
library(sfarrow)
library(leaflet)
library(tidygraph)
library(cppRouting)

# Laster inn vegnettet #
layers <- sf::st_layers("C:/Users/rdn/Documents/Kart/Vegnett/vegnettRuteplan_FGDB_20210401.gdb")
layers$name


#################
### Vegnettet ###
#################

vegnett_parquet_filsti <- paste0("C:/Users/rdn/Documents/Kart/Vegnett/vegnett", aargang, ".parquet")

ds <- arrow::open_dataset(vegnett_parquet_filsti)

vegnett <- ds %>%
  dplyr::filter(FYLKE_ID %in% c("3")) %>%
  sfarrow::read_sf_dataset()

turnrestrictions_geom <- sf::read_sf("C:/Users/rdn/Documents/Kart/Vegnett/vegnettRuteplan_FGDB_20210401.gdb", layer = "ERFKPS_turns") %>%
  # sf::st_transform(crs = 4326)   %>%
  # sf::st_cast("LINESTRING") %>%
  # dplyr::filter((Edge1FID == 1264892 | Edge2FCID == 1264892) | 
  #                 (Edge1FID == 639359 | Edge2FCID == 639360) |
  #                 (Edge1FID == 1267023 | Edge2FCID == 466377)) %>%
  data.frame() %>%
  dplyr::select(Edge1End, Edge1FID, Edge2FID) %>%
  dplyr::filter(Edge1FID == 1264892)


########################################################################
########################## Pakken begynner her########################## 
########################################################################

suppressWarnings(
  vegnett <- vegnett %>%
    sf::st_zm(drop = T) %>%
    dplyr::rename_all(toupper) %>%
    # dplyr::rename(LENGTH = SHAPE_LENGTH) %>% # OBS? Erstatt alle LENGTH med SHAPE_LENGTH
    sf::st_cast("LINESTRING") %>%
    dplyr::rename(FROMNODE = FROMNODEID, # OBS: gjøre likt som 2022 
                  TONODE = TONODEID)
)

rename_geometry <- function(g, name){
  current = attr(g, "sf_column")
  names(g)[names(g)==current] = name
  sf::st_geometry(g)=name
  g
}

vegnett <- rename_geometry(vegnett, "geometry")
sf::st_geometry(vegnett) <- "geometry"

# OBS: skal ikke være med i pakken
vegnett <- vegnett %>%
  sf::st_transform(crs = 4326)


#########################
### Turn restrictions ###
#########################

# OBS: denne gjelder kun for -2021

vegnett_Edge1FID <- vegnett %>%
  data.frame() %>%
  dplyr::filter(FID %in% as.character(unique(turnrestrictions_geom$Edge1FID))) %>%
  dplyr::select(FID, FROMNODE, TONODE) %>%
  dplyr::rename(FROMNODEID_1 = FROMNODE, 
                TONODEID_1 = TONODE)

vegnett_Edge2FID <- vegnett %>%
  data.frame() %>%
  dplyr::filter(FID %in% as.character(unique(turnrestrictions_geom$Edge2FID))) %>%
  dplyr::select(FID, FROMNODE, TONODE) %>%
  dplyr::rename(FROMNODEID_2 = FROMNODE, 
                TONODEID_2 = TONODE)

turnrestrictions_geom_2 <- turnrestrictions_geom %>%
  dplyr::left_join(vegnett_Edge1FID, by = c("Edge1FID" = "FID")) %>%
  dplyr::left_join(vegnett_Edge2FID, by = c("Edge2FID" = "FID")) %>%
  dplyr::mutate(fromToNode = case_when(
    Edge1End == "N" ~ FROMNODEID_1, 
    Edge1End == "Y" ~ TONODEID_1, 
    TRUE ~ ""), 
    toToNode = case_when(
      Edge1End == "N" & FROMNODEID_1 == FROMNODEID_2 ~ TONODEID_2, 
      Edge1End == "Y" & TONODEID_1 == FROMNODEID_2 ~ TONODEID_2, 
      TRUE ~ FROMNODEID_2
    ), 
    turn = 1) %>%
  dplyr::select(fromToNode, toToNode, Edge1End, FROMNODEID_1, TONODEID_1, FROMNODEID_2, TONODEID_2) 

# # 2460182
# test <- turnrestrictions_geom_2 %>%
#   dplyr::filter(!is.na(fromToNode))

# OBS: denne gjelder for alle år
turnrestrictions_geom <- turnrestrictions_geom_2 %>%
  dplyr::rename(FROMNODEID = fromToNode,
                TONODEID = toToNode) %>%
  dplyr::mutate(turn = 1)


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

### OBS: her byttes rekkefølgen på FROMNODEID/TONODEID (disse heter FROMNODE/TONODE i 2022)
# Binding together all the edges #
edges <- rbind(B_FT, FT, B_TF, TF) %>%
  dplyr::mutate(edgeID = c(1:dplyr::n())) %>% # adding new edge ID
  dplyr::mutate(FT_MINUTES = dplyr::case_when( # specify correct FT_MINUTES for edges that go TF
    direction %in% c("B_TF", "TF") ~ TF_MINUTES, TRUE ~ FT_MINUTES), 
    FROMNODEID = case_when(direction %in% c("B_TF", "TF") ~ TONODE, TRUE ~ FROMNODE), 
    TONODEID = case_when(direction %in% c("B_TF", "TF") ~ FROMNODE, TRUE ~ TONODE))

# OBS: kobler på turnrestrictions og fjerner lenkene der kjøreretning ikke er tillatt
edges <- dplyr::left_join(edges, turnrestrictions_geom, by = c("FROMNODEID", "TONODEID")) %>%
  dplyr::filter(is.na(turn))

# OBS: denne trengs ikke?
# edges <- edges %>% 
#   dplyr::filter(is.na(turn)) # %>%
  # dplyr::select(FROMNODEID, TONODEID, LINKID, direction, turn)

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

# OBS: denne fungerer ikke - igraph må ha nodeID-er som starter på 1?
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
graph_cppRouting_SHAPE_LENGTH <- cppRouting::makegraph(edges_SHAPE_LENGTH, directed = T, coords = node_list_coord)



#######################################################################
########################## Pakken stopper her########################## 
#######################################################################

# Bogstadveien eksempel
# Fra-til
# from_node_original <-  632345
# to_node_original <-  632449
# Til-fra
# from_node_original <- 632449
# to_node_original <-  632345

# Annet ex
from_node_original <-  2394232
to_node_original <-  2394213

from_node_original <-  2394213
to_node_original <-  2394232


from_node <- edges %>%
  dplyr::filter(FROMNODEID == from_node_original & direction %in% c("B_FT", "FT")) %>%
  data.frame() %>%
  dplyr::select(FROMNODEID, from)

to_node <- edges %>%
  dplyr::filter(TONODEID == to_node_original & direction %in% c("B_TF", "TF")) %>%
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

test_points <- node_points %>%
  dplyr::filter(nodeID %in% c(unique(from_node$from), 
                              unique(to_node$to))) %>% # 79205
  # dplyr::filter(nodeID %in% c(75379, 60937)) %>%
  # dplyr::filter(nodeID %in%c(4, 6, 1)) %>% # 1473826, 1499638, 1450893
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addMarkers(~long, ~lat, popup = ~as.character(paste(nodeID, ": ", lat, ", ", long)))
# test_points


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



# 
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
