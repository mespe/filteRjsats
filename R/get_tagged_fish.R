#' Get A Dataframe of All Tagged fish from CalFishTrack
#'
#' This function searches the California Fish Tracking ERDAPP Database to create
#' a dataframe of tagged fish data. Desired fields can be set to select only
#' certain fields. This data is used to add fish data to detection data in the
#' add_fish function.
#'
#' @param important_fields a vector of important field indexes to be imported
#' from the ERDDAP dataset
#' @returns A dataframe of fish data which can be joined to detection data
#' @export
#' @examples
#'# Retrieve only a few important fields (fish type, tag code, release date)
#'tout <- getOption("timeout")
#'options(timeout = 4)
#'fields <- c(7,8,16)
#'try(cal_fish_lite <- get_tagged_fish(important_fields = fields))
#'options(timeout = tout)
get_tagged_fish <- function(important_fields = NULL){
  rerddap::cache_delete_all()
  info <- rerddap::info('FED_JSATS_taggedfish',
                        url = 'https://oceanview.pfeg.noaa.gov/erddap')
  if(!is.null(important_fields))
  {fields <- info$variables$variable_name[important_fields]}
  else {fields = NULL}

  fish <- rerddap::tabledap('FED_JSATS_taggedfish',
                            url = "https://oceanview.pfeg.noaa.gov/erddap/",
                            fields = fields)
  fish <- dplyr::distinct(fish)
  if(!is.null(fish$tag_id_hex)) fish$Tag_Hex = as.character(fish$tag_id_hex)
  if(!is.null(fish$fish_release_date))
    fish$fish_release_date = lubridate::mdy_hms(fish$fish_release_date,
                                                tz = "Etc/GMT+8")
  if(!is.null(fish$release_river_km))
    fish$release_rkm = as.numeric(fish$release_river_km)
  if(!is.null(fish$tag_warranty_life))
    fish$tag_life = lubridate::days(as.integer(fish$tag_warranty_life))
  if(!is.null(fish$fish_length)) fish$length = as.numeric(fish$fish_length)
  if(!is.null(fish$fish_weight)) fish$weight = as.numeric(fish$fish_weight)
  if(!is.null(fish$release_latitude))
    fish$release_latitude = as.numeric(fish$release_latitude)
  if(!is.null(fish$release_longitude))
    fish$release_longitude = as.numeric(fish$release_longitude)
  as.data.frame(fish)
}
