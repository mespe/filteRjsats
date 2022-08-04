get_reference_tags <- function(){
  if (!requireNamespace("pkg", quietly = TRUE)) {
    stop(
      "Package \"pkg\" must be installed to use this function.",
      call. = FALSE
    )
  }
  reference_tags <- rerddap::tabledap('FED_JSATS_receivers',
                                      url = "https://oceanview.pfeg.noaa.gov/erddap/",
                                      fields = c("receiver_beacon_id_hex",
                                                 "receiver_beacon_pri"))
  reference_tags <- dplyr::distinct(.data = reference_tags, receiver_beacon_id_hex)
  reference_tags$receiver_beacon_id_hex
}

get_fish_fields <- function(){
  if (!requireNamespace("pkg", quietly = TRUE)) {
    stop(
      "Package \"pkg\" must be installed to use this function.",
      call. = FALSE
    )
  }
  info <- rerddap::info('FED_JSATS_taggedfish',
                        url = 'https://oceanview.pfeg.noaa.gov/erddap')
  info$variables
}

get_tagged_fish <- function(important_fields = NULL){
  if (!requireNamespace("pkg", quietly = TRUE)) {
    stop(
      "Package \"pkg\" must be installed to use this function.",
      call. = FALSE
    )
  }
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
  fish$Tag_Hex = as.character(fish$tag_id_hex)
  fish$fish_release_date = lubridate::mdy_hms(fish$fish_release_date, tz = "Etc/GMT+8")
  fish$release_rkm = as.numeric(fish$release_river_km)
  fish$tag_life = lubridate::days(as.integer(fish$tag_warranty_life))
  fish$length = as.numeric(fish$fish_length)
  fish$weight = as.numeric(fish$fish_weight)
  fish$release_latitude = as.numeric(fish$release_latitude)
  fish$release_longitude = as.numeric(fish$release_longitude)
  as.data.frame(fish)
}

get_rcvr_fields <- function(){
  if (!requireNamespace("pkg", quietly = TRUE)) {
    stop(
      "Package \"pkg\" must be installed to use this function.",
      call. = FALSE
    )
  }
  info <- rerddap::info('FED_JSATS_receivers',
                        url = 'https://oceanview.pfeg.noaa.gov/erddap')
  info$variables
}

rcvr_fields = c("dep_id",
                "receiver_serial_number",
                "latitude",
                "longitude",
                "receiver_location",
                "receiver_river_km",
                "receiver_general_location",
                "receiver_general_river_km",
                "receiver_beacon_id_hex",
                "receiver_start",
                "receiver_end")

get_rcvr_data <- function(fields = NULL){
  if (!requireNamespace("pkg", quietly = TRUE)) {
    stop(
      "Package \"pkg\" must be installed to use this function.",
      call. = FALSE
    )
  }
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
