#' @export
add_fish <- function(prefilter_file, fish){
  fish_tmp <- fish[fish$fish_release_date > (min(prefilter_file$DateTime_Local)-lubridate::days(200)),]
  prefilter_file$TagInFile = prefilter_file$Tag_Hex %in% fish_tmp$Tag_Hex #Check for Study Tags
  message(paste0("Detections in File: ",length(prefilter_file$DateTime_Local)))
  file <- prefilter_file[prefilter_file$TagInFile == TRUE,]
  file <- dplyr::left_join(file,fish_tmp, by= c("Tag_Hex" = "tag_id_hex"))
  file$CheckDT = ifelse(file$DateTime_Local > file$fish_release_date,
                        TRUE, #Detections after release
                        FALSE) #Detections before release
  file$CheckBattLife = ifelse(file$DateTime_Local <= file$fish_release_date+(2*file$tag_life),
                              TRUE, #Detections before tag failure
                              FALSE) #Detections after tag failure
  file <- file[file$CheckDT == TRUE & file$CheckBattLife == TRUE,]
  fish_file <- file

  if(length(file$DateTime_Local) > 1) {
    message(paste0("Number of Valid Tag IDs: ", length(unique(file$Tag_Hex))))
    message(paste0("Number of Valid Detections: ", length(file$DateTime_Local)))
  } else {
    message("No Valid Detections")}

  fish_file
}
