
convert_nodeID <- function(node_original, 
                           from_to = "from") {
  
  if (from_to == "from"){
  from_node <- edges %>%
    dplyr::filter(FROMNODEID == node_original) %>%
    data.frame() %>%
    dplyr::select(FROMNODEID, from)
  }
  
  if (from_to == "to"){
  to_node <- edges %>%
    dplyr::filter(TONODEID == to_node_original) %>%
    data.frame() %>%
    dplyr::select(TONODEID, to)
  }
}
