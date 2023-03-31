#' Get A List of Reference (Beacon) Tags from ERDAPP
#'
#' This function searches the California Fish Tracking ERDAPP Database to create
#' a list of beacon tag hexadecimal IDs. Used in the prefilter to separate
#' beacon tags from tagged fish.
#'
#' @return A vector of beacon tags hexadecimal IDs
#' @export
get_reference_tags <- function(){
  reference_tags <- rerddap::tabledap('FED_JSATS_receivers',
                                      url = "https://oceanview.pfeg.noaa.gov/erddap/",
                                      fields = c("receiver_beacon_id_hex",
                                                 "receiver_beacon_pri"))
  reference_tags <- dplyr::distinct(.data = reference_tags, receiver_beacon_id_hex)
  reference_tags$receiver_beacon_id_hex
}
#' @examples
#' # Download reference tags from CalFishTrack
#' ref_tags <- get_reference_tags()
#' head(ref_tags)
#'

#' Get A List of Fish Related Fields from ERDAPP
#'
#' This function searches the California Fish Tracking ERDAPP Database to create
#' a list of all potential fields related to fish metrics. Used to identify
#' important fish data fields to add to detection data in the add_fish function.
#'
#' @return A vector of potential fish fields which the user may review to
#' retrieve specific field indices
#' @export
get_fish_fields <- function(){
  info <- rerddap::info('FED_JSATS_taggedfish',
                        url = 'https://oceanview.pfeg.noaa.gov/erddap')
  info$variables
}
#' @examples
#' fish_fields <- get_fish_fields
#'

#' Get A Dataframe of All Tagged fish from CalFishTrack
#'
#' This function searches the California Fish Tracking ERDAPP Database to create
#' a dataframe of tagged fish data. Desired fields can be set to select only
#' certain fields. This data is used to add fish data to detection data in the
#' add_fish function.
#'
#' @param important_fields a vector of important field indexes to be imported
#' from the ERDDAP dataset
#' @return A dataframe of fish data which can be joined to detection data
#' @export
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
#' @examples
#' # Retrieve all fields from CalFishTrack tagged fish table
#' calfish <- get_tagged_fish()
#'
#' # Retrieve only a few important fields (fish type, tag code, release date)
#' fields <- c(7,8,16)
#' cal_fish_lite <- get_tagged_fish(important_fields = fields)
#'

#' Example fish data from CalFishTrack
#'
#' A dataframe of acoustically tagged fish downloaded from ERDDAP representing
#' fish released in 2021 and 2022.
#'
#' @format A dataframe of example acoustically tagged fish metadata
"cft_fish"

#' Get A List of Receiver Related Fields from ERDAPP
#'
#' This function searches the California Fish Tracking ERDAPP Database to create
#' a list of all potential fields related to acoustic receiver metadata.
#' Used to identify important metadata fields to include when adding receiver
#' data in the join_rcvr_data function.
#'
#' @return A vector of potential receiver metadata fields which the user may review
#' @export
get_rcvr_fields <- function(){
  info <- rerddap::info('FED_JSATS_receivers',
                        url = 'https://oceanview.pfeg.noaa.gov/erddap')
  info$variables
}
#' @examples
#' # View a list of available receiver fields
#' get_rcvr_fields()
#'

#' A List of Important Receiver Related Fields from ERDAPP
#'
#' A vector of acoustic receiver metadata fields which are needed, at a minimum,
#' to run the join_rcvr_data function
#'
#' @format A vector of potential receiver metadata fields which the user may review
"rcvr_fields"

#' Get A Dataframe of All Receiver Data from CalFishTrack
#'
#' This function searches the California Fish Tracking ERDAPP Database to create
#' a dataframe ofacoustic receiver metadata. Desired fields can be set to select
#' only certain fields, object rcvr_fields is the default. This data is used to add
#' receiver metadata to detection data in the add_fish function.
#'
#' @param fields a vector of important field names to be imported from the
#' ERDDAP dataset
#' @return A dataframe of receiver metadata which can be joined to detection data
#' @export
get_rcvr_data <- function(fields = rcvr_fields){
  rerddap::cache_delete_all()
  info <- rerddap::info('FED_JSATS_receivers',
                        url = 'https://oceanview.pfeg.noaa.gov/erddap')
  if(!is.null(fields))
  {fields <- rcvr_fields}
  else {fields = NULL}

  unique_receivers <- rerddap::tabledap('FED_JSATS_receivers',
                                        url = "https://oceanview.pfeg.noaa.gov/erddap/",
                                        fields = fields)
  unique_receiver <- dplyr::distinct(.data = unique_receivers)
  unique_receiver$latitude = as.numeric(unique_receiver$latitude)
  unique_receiver$longitude = as.numeric(unique_receiver$longitude)
  unique_receiver$receiver_river_km = as.numeric(unique_receiver$receiver_river_km)
  unique_receiver$receiver_general_river_km = as.numeric(unique_receiver$receiver_general_river_km)
  unique_receiver$receiver_start <- lubridate::mdy_hms(unique_receiver$receiver_start)
  unique_receiver$receiver_end <- lubridate::mdy_hms(unique_receiver$receiver_end)
  unique_receiver$receiver_serial_number = as.character(unique_receiver$receiver_serial_number)
  unique_receiver <- dplyr::rename(.data = unique_receiver,
                                   "ReceiverSN" = receiver_serial_number)
  unique_receiver$regional_location = dplyr::case_when(
    unique_receiver$receiver_general_location == "Benicia_east" ~ "Benicia_East",
    unique_receiver$receiver_general_location == "Benicia_west" ~ "Benicia_West",
    unique_receiver$receiver_general_location %in% c("CVP_Trash_Rack_1","CVP_UpStream_TrashRack", "CVP_Trash_Rack_US", "CVP_Trash_Rack_DS") ~ "CVP_Trash_Rack",
    unique_receiver$receiver_general_location %in% c("GC_D") ~ "Grant_Line_Ctrl_DS",
    unique_receiver$receiver_general_location %in% c("GC_U") ~ "Grant_Line_Ctrl_US",
    unique_receiver$receiver_general_location %in% c("Grant_Line_DS") ~ "Grant_Line_DS",
    unique_receiver$receiver_general_location %in% c("Grant_Line_US") ~ "Grant_Line_US",
    unique_receiver$receiver_general_location %in% c("Old_River_Tracy_D") ~ "Old_River_Tracy_DS",
    unique_receiver$receiver_general_location %in% c("Old_River_Tracy_U1","Old_River_Tracy_U2") ~ "Old_River_Tracy_US",
    unique_receiver$receiver_location         %in% c("CC_RGD_1_J","CC_RGD_2_J","CC_RGD1_J","CC_RGD2_J") ~ "SWP_radial_gates_DS",
    unique_receiver$receiver_location         %in% c("CC_RGU_1_J","CC_RGU_2_J","CC_RGU1_J","CC_RGU2_J") ~ "SWP_radial_gates_DS",
    unique_receiver$receiver_general_location == "CVP_Tank" ~ "CVP_Tank",
    unique_receiver$receiver_general_location == "Holland_Cut_Quimby" ~ "Holland_Cut_Quimby",
    unique_receiver$receiver_general_location == "I80-50_Br" ~ "I80-50_Br",
    unique_receiver$receiver_general_location == "MiddleRiver" ~ "MiddleRiver",
    unique_receiver$receiver_general_location == "MidR_OR_B_2021" ~ "Middle_River_at_Old_River",
    unique_receiver$receiver_general_location == "Old River" ~ "Old_River",
    unique_receiver$receiver_general_location == "Old_River_Quimby" ~ "Old_River_Quimby",
    unique_receiver$receiver_general_location == "Sac_BlwGeorgiana" ~ "Sac_BlwGeorgiana",
    unique_receiver$receiver_general_location == "Sac_BlwGeorgiana2" ~ "Sac_BlwGeorgiana2",
    unique_receiver$receiver_general_location %in% c("SWP_intake","Clifton_Court_SWP")  ~ "SWP_intake",
    unique_receiver$receiver_general_location == "SWP_radial_gates_DS" ~ "SWP_radial_gates_DS",
    unique_receiver$receiver_general_location %in% c("SWP_radial_gates_US", "Clifton_Court_RadGates", "SWP_radial_gates") ~ "SWP_radial_gates_US",
    unique_receiver$receiver_general_location == "TowerBridge" ~ "TowerBridge",
    unique_receiver$receiver_general_location == "Durhamferry" ~ "Durham_Ferry",
    unique_receiver$receiver_general_location == "HOR" ~ "SJ_Head_of_Old_River",
    unique_receiver$receiver_general_location == "MidR_hwy4_DS" ~ "Middle_River_Hwy4_US",
    unique_receiver$receiver_general_location == "MidR_hwy4_US" ~ "Middle_River_Hwy4_DS",
    unique_receiver$receiver_general_location == "OR_HOR_DS" ~ "OR_Head_of_Old_River_DS",
    unique_receiver$receiver_general_location == "OR_HOR_US" ~ "OR_Head_of_Old_River_US",
    unique_receiver$receiver_general_location == "OR_hwy4_US" ~ "Old_River_Hwy4_US",
    unique_receiver$receiver_general_location == "OR_hwy4_DS" ~ "Old_River_Hwy4_DS",
    unique_receiver$receiver_general_location == "ORMR" ~ "Old_River_at_Middle_River",
    unique_receiver$receiver_general_location %in% c("RT_MiddleRiver","MiddleRiver") ~ "MiddleRiver_RR_Bridge",
    unique_receiver$receiver_general_location == "RT_OldRiver" ~ "OldRiver_RR_Bridge",
    unique_receiver$receiver_general_location == "SJ_BCA" ~ "SJ_BCA",
    unique_receiver$receiver_general_location == "Mossdale" ~ "Mossdale",
    unique_receiver$receiver_general_location %in% c("SJ_HOR_DS","SJ_HOR_US") ~ "SR_Head_of_Old_River")
  data.frame(unique_receiver)
}
#' @examples
#' # Retrieve the default variables from Cal Fish Track
#' get_rcvr_data()
#'
#' # Retrieve all receiver metadata fields
#' get_rcvr_data(fields = c(1:22))
#'

#' Example receiver data from CalFishTrack
#'
#' A dataframe of acoustic receiver metadata downloaded from ERDDAP representing
#' receivers deployed from 2021-2022.
#'
#' @format A dataframe of example acoustic receiver metadata
"cft_rcvrs"
