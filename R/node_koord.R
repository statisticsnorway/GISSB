
#' Knytte koordinater til noder i vegnettet
#'
#' Funksjon for å finne nærmeste punkter (noder) i vegnettet til angitte koordinater.
#'
#' @param koords sf-objekt med koordinatene man ønsker å plassere på vegnettet.
#' @param fra_til Karaktervektor med "fra" dersom noden skal være et startpunkt eller "til" dersom noden skal være et stoppunkt.
#' @param ID_col Karaktervektor med navnet på ID-kolonnen.
#' @param crs_out Numerisk vektor for ønsket koordinatsystem.
#' @param knn Numerisk vektor med antall noder per som blir returnert per koordinat. Dersom knn = 1 returneres den nærmeste noden til kooordinatene, dersom knn = 2 returneres de to nærmeste nodene til koordinatene osv.
#' @param membership Boolsk. Dersom TRUE begrenses søket etter noder til å kun gjelde noder som tilhører et tilknyttet vegnettverk til fra- eller tilpunktet (kun mulig for enten fra eller til).
#'
#'
#' @returns Objekt (data.frame) med kolonnene from_node/to_node, membership, coords_google_from_node/coords_google_to_node, nabor og ID
#' @export
#'
#' @examples
#' fra <- adresse_api_koord(postnummer = "0177",
#'                          adresse = "Akersveien 26")
#' from_node <- node_koord(koords = fra, fra_til = "fra")
#'
#' til <- adresse_api_koord(postnummer = "2211",
#'                          adresse = "Otervegen 23")
#' to_node <- node_koord(koords = til, fra_til = "til")
#' @encoding UTF-8
#'
#'

node_koord <- function(koords,
                       fra_til = "fra",
                       ID_col = "ID",
                       crs_out = 25833,
                       knn = 1,
                       membership = F) {

  if (fra_til == "fra"){

    nodes_start <- nodes %>%
      dplyr::filter(nodeID %in% unique(edges$from))

    # # OBS
    if (membership == T){
      nodes_start <- nodes_start %>%
        dplyr::filter(membership %in% unique(to_node$membership))
    }

    coords_start <- nodes_start %>%
      sf::st_coordinates()

    fra_koord <- koords %>%
      sf::st_coordinates() %>%
      matrix(ncol = 2)
    colnames(fra_koord) <- c("X", "Y")

    node_index_o <- nabor::knn(data = coords_start,
                               query = fra_koord,
                               k = knn)

    nodes_start <- sf::st_transform(nodes_start, crs = 4326) %>%
      koords_to_google() %>%
      data.frame() %>%
      dplyr::rename(from_node = nodeID,
                    coords_google_from_node = coords_google) %>%
      dplyr::select(-geometry)

    start_node <- nodes_start[node_index_o$nn.idx, ]
    start_node$knn <- rep(1:knn, each=nrow(koords))

    # ID <- koords$ID
    ID <- koords %>%
      dplyr::select(!!as.name(ID_col)) %>%
      data.frame() %>%
      select(-geometry)

    dists <-  data.frame(ID, node_index_o$nn.dists)

    # return(dists)

    dists <- reshape2::melt(dists, id.vars = ID_col,
                            variable.name = "variabel",
                            value.name = "avstand_koord_node_from") %>%
      dplyr::select(-variabel)

    start_node <- cbind(start_node, dists)

    return(start_node)

  }

  if (fra_til == "til"){

    nodes_end <- nodes %>%
      dplyr::filter(nodeID %in% unique(edges$to))

    if (membership == T){
      nodes_end <- nodes_end %>%
        dplyr::filter(membership %in% unique(from_node$membership))
    }

    coords_end <- nodes_end %>%
      sf::st_coordinates()

    til_koord <- koords %>%
      sf::st_coordinates() %>%
      matrix(ncol = 2)
    colnames(til_koord) <- c("X", "Y")

    node_index_d <- nabor::knn(data = coords_end,
                               query = til_koord,
                               k = 1)

    nodes_end <- sf::st_transform(nodes_end, crs = 4326) %>%
      koords_to_google() %>%
      data.frame() %>%
      dplyr::rename(to_node = nodeID,
                    coords_google_to_node = coords_google) %>%
      dplyr::select(-geometry)

    end_node <- nodes_end[node_index_d$nn.idx, ]
    end_node$knn <- rep(1:knn, each=nrow(koords))

    # ID <- koords$ID
    ID <- koords %>%
      dplyr::select(!!as.name(ID_col)) %>%
      data.frame() %>%
      select(-geometry)

    dists <-  data.frame(ID, node_index_d$nn.dists)

    # return(dists)

    dists <- reshape2::melt(dists, id.vars = ID_col,
                            variable.name = "variabel",
                            value.name = "avstand_koord_node_to") %>%
      dplyr::select(-variabel)

    end_node <- cbind(end_node, dists)

    return(end_node)


  }

}



