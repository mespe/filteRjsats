#' Get A List of Reference (Beacon) Tags from ERDAPP
#'
#' This function searches the California Fish Tracking ERDAPP Database to create
#' a list of beacon tag hexadecimal IDs. Used in the prefilter to separate
#' beacon tags from tagged fish.
#'
#' @returns A vector of beacon tags hexadecimal IDs
#' @export
#' @examples
#' # Download reference tags from CalFishTrack
#' tout <- getOption("timeout")
#' options(timeout = 4)
#' try(ref_tags <- get_reference_tags())
#' options(timeout = tout)
get_reference_tags <- function(){
  reference_tags <- rerddap::tabledap('FED_JSATS_receivers',
                                      url = "https://oceanview.pfeg.noaa.gov/erddap/",
                                      fields = c("receiver_beacon_id_hex",
                                                 "receiver_beacon_pri"))
  reference_tags <- dplyr::distinct(.data = reference_tags, receiver_beacon_id_hex)
  reference_tags$receiver_beacon_id_hex
}
