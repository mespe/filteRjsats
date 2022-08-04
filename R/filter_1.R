#' Apply the "prefilter" to a Detection Dataframe
#'
#' This function takes a detection dataframe output from read_jsats and filters
#' out multipath signals (time between detections < 0.3 seconds) and spurious
#' signals which donot occur within a specified time frame of the last
#' detection. For beacon tags, this time frame is 192 seconds, and for fish,
#' this time is 120 seconds. Following this, the dataframes are standardized so
#' that all detection dataframes from any technology type are identical and
#' superfluous fields are removed.
#'
#' @param jstats_file A dataframe which is the output from read_jstats()
#' @param reference_tags A vector of potential reference (beacon) tag IDs
#' @return A standardized detection dataframe
#' @export
prefilter <- function(jsats_file, reference_tags){
  temp <- jsats_file
  temp <- dplyr::arrange(.data = temp,H)
  temp <- dplyr::group_by(.data = temp, H)
  temp <- dplyr::arrange(.data = temp, H, DT)
  det_count <- dplyr::summarise(temp, det_count = dplyr::n())
  temp <- dplyr::left_join(temp, det_count, by = "H")
  temp <- temp[temp$det_count > 1,]
  temp$time_diff_lag = difftime(temp$DT, dplyr::lag(temp$DT),
                                units = "secs")
  temp$multipath = ifelse(temp$time_diff_lag > lubridate::seconds(0.3)|
                            temp$H != dplyr::lag(temp$H), FALSE, TRUE)
  temp$multipath = ifelse(is.na(temp$time_diff_lag) &
                            dplyr::lead(temp$multipath) == FALSE,
                          FALSE,
                          ifelse(is.na(temp$time_diff_lag),
                                 TRUE,
                                 temp$multipath))
  temp <- temp[temp$multipath == FALSE,] # filter out Multipath
  temp$time_diff_lag = difftime(temp$DT, dplyr::lag(temp$DT),
                                units = "secs")
  temp$time_diff_lag = ifelse(temp$H != dplyr::lag(temp$H),NA,temp$time_diff_lag)
  temp$RefTag = ifelse(temp$D %in% reference_tags, TRUE, FALSE) #Is it a ref tag?
  temp$CheckMBP = ifelse(temp$RefTag == TRUE, # If a ref tag,
                         (temp$time_diff_lag < lubridate::seconds(3*64)), # 2 hits in 3*Max PRI
                         (temp$time_diff_lag < lubridate::seconds(12*10))) # 2 hits in 12*Max PRI
  temp$CheckMBP = ifelse(is.na(temp$time_diff_lag) & dplyr::lead(temp$CheckMBP) == FALSE, #First Detection is FALSE if 2nd is FALSE
                         FALSE,
                         ifelse(is.na(temp$time_diff_lag),
                                TRUE,
                                temp$CheckMBP)) #Otherwise it's valid
  temp$CheckMBP = ifelse(temp$CheckMBP == FALSE & !is.na(dplyr::lead(temp$time_diff_lag)) &
                           dplyr::lead(temp$time_diff_lag) < lubridate::seconds(12*10), #If invalid based on last detection, but following detection is <120s
                         TRUE, #Valid
                         temp$CheckMBP) #Return Previous Assignment
  temp <- temp[temp$CheckMBP == TRUE,]
  det_count <- dplyr::summarise(temp, det_count = dplyr::n())
  temp <- dplyr::left_join(temp, det_count, by = "H")
  temp <- temp[temp$det_count.y > 1,]
  temp$DateTime_Local = temp$DT
  temp$ReceiverSN = temp$R
  temp$Tag_Decimal = temp$D
  temp$Tag_Hex = temp$H
  temp <- dplyr::ungroup(temp)
  temp <- dplyr::select(.data =  temp,
                        any_of(c("ReceiverSN", "Make", "DateTime_Local", "Tag_Decimal", "Tag_Hex", "Tilt",
                                 "Volt", "Temp", "SigStr", "Freq", "Thres", "CheckMBP")))
  temp <- dplyr::arrange(temp, ReceiverSN, Tag_Hex, DateTime_Local)
  temp <- temp[!is.na(temp$DateTime_Local),]
  prefilter_file <- temp
  prefilter_file
}
