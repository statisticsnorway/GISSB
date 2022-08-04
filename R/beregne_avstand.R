

#' Beregne korteste kjøreavstand (igraph)
#'
#' Funksjon for å beregne korteste kjøreavstand (i minutter eller meter) mellom noder i vegnettet. Funksjonen kan også returnere nodelenken (path) som den korteste kjøreruten består av.
#' For å bruke funksjonen må vegnettet ha blitt lastet inn og omgjort til et tbl_graph-objekt som heter "graph". Dette gjøres med funksjonen vegnett_to_R.
#'
#' @param from_node Numerisk verdi med node-ID (for en vektor med flere node-ID-er, se funksjonen beregne_avstand_cpp).
#' @param to_node Numerisk verdi med node-ID (for en vektor med flere node-ID-er, se funksjonen beregne_avstand_cpp).
#' @param enhet Karaktervektor med "FT_MINUTES" for å få korteste kjørerute målt i minutter eller "LENGTH" for korteste kjørerute målt i meter.
#' @param path Boolsk. Dersom path = TRUE returneres nodelenken som den korteste kjøreruten består av.
#'
#' @returns Vektor med korteste kjørerute i minutter eller meter. Dersom path = T returneres nodelenken som den korteste kjøreruten består av.
#' @export
#'
#' @examples
#' avstand_min <- beregne_avstand(from_node = 26956,
#'                                to_node = 210373,
#'                                enhet = "FT_MINUTES")
#'
#' avstand_meter <- beregne_avstand(from_node = 26956,
#'                                  to_node = 210373,
#'                                  enhet = "LENGTH")
#'
#' path <- beregne_avstand(from_node = 26956,
#'                         to_node = 210373,
#'                         enhet = "FT_MINUTES",
#'                         path = T)
#' @encoding UTF-8
#'
#'

beregne_avstand <- function(from_node,
                            to_node,
                            enhet = "FT_MINUTES",
                            path = F) {

  path_graph <- igraph::shortest_paths(
    graph = graph,
    from = from_node,
    to = to_node,
    output = 'both',
    weights = graph %>% tidygraph::activate(edges) %>% pull(!!as.name(enhet))

  )

  path_graph_length <- graph %>%
    igraph::subgraph.edges(eids = path_graph$epath %>%
                             unlist()) %>%
    tidygraph::as_tbl_graph()

  avstand <- path_graph_length %>%
    tidygraph::activate(edges) %>%
    tibble::as_tibble() %>%
    dplyr::summarise(length = sum(!!as.name(enhet)))


  if (path == T) {
    return(path_graph)
  } else {
    return(avstand)
  }

}
