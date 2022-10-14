
library(sf)
library(tidyverse)
library(sfarrow)
library(leaflet)
library(tidygraph)
library(cppRouting)

source("C:/Users/rdn/Documents/Github/GISSB/experimental/vegnett_to_R (TR).R", encoding = "UTF-8")

aargang <- 2021

# Laster inn vegnettet #
vegnett_parquet_filsti <- paste0("C:/Users/rdn/Documents/Kart/Vegnett/vegnett", aargang, ".parquet")

ds <- arrow::open_dataset(vegnett_parquet_filsti)
if (aargang <= 2021) {
  vegnett <- ds %>%
    dplyr::filter(FYLKE_ID %in% c("3")) %>%
    sfarrow::read_sf_dataset()

  turnrestrictions_geom <- sf::read_sf("C:/Users/rdn/Documents/Kart/Vegnett/vegnettRuteplan_FGDB_20210401.gdb", layer = "ERFKPS_turns") %>%
    data.frame() %>%
    dplyr::select(Edge1End, Edge1FID, Edge2FID)

  vegnett_list <- vegnett_to_R(vegnett = vegnett,
                               fromnodeID = "FROMNODEID",
                               tonodeID = "TONODEID",
                               FT_minutes = "FT_MINUTES",
                               TF_minutes = "TF_MINUTES",
                               meters = "SHAPE_LENGTH",
                               year = aargang,
                               turn_restrictions = TRUE)


}

if (aargang == 2022){
  vegnett <- ds %>%
    dplyr::filter(municipality == "301") %>%
    sfarrow::read_sf_dataset()

  turnrestrictions_geom <- sf::read_sf("C:/Users/rdn/Documents/Kart/Vegnett/vegnettRuteplan_FGDB_20220703.gdb", layer = "turnrestrictions_geom")
  turnrestrictions_geom <- turnrestrictions_geom %>%
    dplyr::filter(!(fromFromNode == "1450893" & fromToNode == "1499638" & toToNode == "1473826")) # OBS: muligens feil?

  vegnett_list <- vegnett_to_R(vegnett = vegnett,
                               FT_minutes = "DRIVETIME_FW",
                               TF_minutes = "DRIVETIME_BW",
                               meters = "SHAPE_LENGTH",
                               year = aargang,
                               turn_restrictions = TRUE)
}


graph <- vegnett_list[[1]]
nodes <- vegnett_list[[2]]
edges <- vegnett_list[[3]]
graph_cppRouting_minutes <- vegnett_list[[4]]
graph_cppRouting_meters <- vegnett_list[[5]]

if (aargang == 2021) {
### Bogstadveien eksempel 2021
## Fra-til
from_node_original <-  632345
to_node_original <-  632449
## Til-fra
# from_node_original <- 632449
# to_node_original <-  632345

### Sannergata 2021
# from_node_original <- 634745
# to_node_original <- 2674120
# from_node_original <- 2674120
# to_node_original <- 634745
}

if (aargang == 2022) {
### Bogstadveien eksempel 2022
from_node_original <- 1473826
to_node_original <- 1450893
# from_node_original <- 1450893
# to_node_original <- 1473826
}

from_node <- edges %>%
  dplyr::filter(FROMNODEID_new == from_node_original & direction %in% c("B_FT", "FT")) %>%
  data.frame() %>%
  dplyr::select(FROMNODEID_new, from, direction)

to_node <- edges %>%
  dplyr::filter(TONODEID_new == to_node_original & direction %in% c("B_TF", "TF")) %>%
  data.frame() %>%
  dplyr::select(TONODEID_new, to, direction)

path <- GISSB::shortest_path_igraph(from_node_ID = unique(from_node$from),
                                    to_node_ID = unique(to_node$to),
                                    unit = "minutes",
                                    path = TRUE)
GISSB::path_leaflet(path)


###

node_points <- nodes %>%
  sf::st_transform(crs = 4326)

node_points <-node_points %>%
  dplyr::mutate(long = unlist(map(node_points$geometry,1)),
                lat = unlist(map(node_points$geometry,2)))

test_points <- node_points %>%
  dplyr::filter(nodeID %in% c(unique(from_node$from), unique(to_node$to))) %>%
  # dplyr::filter(nodeID == 54507) %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addMarkers(~long, ~lat, popup = ~as.character(paste(nodeID, ": ", lat, ", ", long)))
# test_points

