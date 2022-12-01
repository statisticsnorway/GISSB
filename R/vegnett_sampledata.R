#' Sample data
#'
#' Sample of the Norwegian Road Network (`vegnettRuteplan_FGDB_20210528.gdb`) downloaded from Geonorge. Only the first 1000 rows from Oslo county are included.
#'
#' @docType data
#'
#' @usage data(vegnett_sampledata)
#'
#' @format An object of class `sf`.
#'
#' @keywords datasets
#'
#' @source \href{https://kartkatalog.geonorge.no/metadata/nvdb-ruteplan-nettverksdatasett/8d0f9066-34f9-4423-be12-8e8523089313/}{National roads database - road network for routing}
#'
#' @examples
#' data(vegnett_sampledata)
#' class(vegnett_sampledata)
#' head(vegnett_sampledata)
"vegnett_sampledata"
