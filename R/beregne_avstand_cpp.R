
#' Beregne korteste kjøreavstand (cppRouting)
#'
#' Funksjon for å beregne korteste kjøreavstand (i minutter eller meter) mellom to eller flere noder i vegnettet. Fungerer også med vektorer med flere adresser for start_node og/eller to_node.
#' For å bruke funksjonen må vegnettet ha blitt lastet inn og omgjort til et cppRouting-objekt som heter "graph_cppRouting_FT_MINUTES" eller "graph_cppRouting_LENGTH". Dette gjøres med funksjonen vegnett_to_R.
#'
#' @param from_node_ID Numerisk vektor med én eller flere node-ID-er.
#' @param to_node_ID Numerisk vektor med én eller flere node-ID-er.
#' @param enhet Karaktervektor med "FT_MINUTES" for å få korteste kjørerute målt i minutter eller "LENGTH" for korteste kjørerute målt i meter.
#' @param dist Karaktervektor som angir om man ønsker alle kjørerutene per from_node. Dersom man kun ønsker det korteste kjøreruten per from_node angir man dist = "min". "max" angir den lengste kjøreruten per from_node.
#'
#' @returns Objekt (data.frame) med hvor mange minutter eller meter kjøreavstand det er mellom de angitte fra- og tilpunktene.
#' @export
#'
#' @examples
#' avstand_cpp_min <- beregne_avstand_cpp(26956,
#'                                        210373,
#'                                        enhet = "FT_MINUTES")
#'
#' avstand_cpp_meter <- beregne_avstand_cpp(26956,
#'                                          210373,
#'                                          enhet = "LENGTH")
#' @encoding UTF-8
#'
#'

beregne_avstand_cpp <- function(from_node_ID,
                                to_node_ID,
                                enhet = "FT_MINUTES",
                                dist = "all") {

  if (enhet == "FT_MINUTES") {
    graph_cppRouting <- graph_cppRouting_FT_MINUTES
  }

  if (enhet == "LENGTH") {
    graph_cppRouting <- graph_cppRouting_LENGTH
  }


  dists <- cppRouting::get_distance_matrix(graph_cppRouting,
                                           from = from_node_ID,
                                           to = to_node_ID,
                                           algorithm = "phast")
  dists2 <- data.frame(dists)

  dists2 <- tibble::rownames_to_column(dists2, "from_node")
  dists2_long <- reshape2::melt(dists2, id.vars = "from_node",
                                variable.name = "to_node",
                                value.name = "length")
  dists2_long$to_node <- gsub("X", "", dists2_long$to_node)

  if (dist == "min") {
    # Beholder kun den nærmeste fødeavdelingen #
    dists2_long <- dists2_long %>%
      dplyr::group_by(from_node) %>%
      # dplyr::slice(which.min(minutter)) %>%
      dplyr::slice(which.min(length)) # %>% #OBS?
    # dplyr::right_join(from_node, by = c("from_node"))
  }

  if (dist == "max") {
    # Beholder kun fødeavdelingen som er lengst unna #
    dists2_long <- dists2_long %>%
      dplyr::group_by(from_node) %>%
      # dplyr::slice(which.min(minutter)) %>%
      dplyr::slice(which.max(length)) # %>% #OBS?
    # dplyr::right_join(from_node, by = c("from_node"))
  }

  dists2_long$from_node <- as.integer(dists2_long$from_node)
  dists2_long$to_node <- as.integer(dists2_long$to_node)

  return(dists2_long)

}
