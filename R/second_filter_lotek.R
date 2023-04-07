#' Four Hit Filter for Lotek Detections
#'
#' This function takes a Lotek detection dataframe generated from the add_fish()
#' function and filters it a second time to remove any remaining multipath
#' detections, and then check the remaining detections by comparing the time
#' between detections, for a rolling window of 4 detections to ensure it is less
#' 16.6x the stated pulse rate interval. It additionally checks that all
#' detections within the window occur within 20% of the pulse rate interval of
#' the other detections and that the standard deviation of pulse rate intervals
#' is less than 0.025. Called by second_filter().
#'
#' @param fish_file a dataframe of detections retrieved from add_fish()
#' @returns A dataframe which has been filtered to remove false positives
#' @export
#' @examples
#' # Apply the Lotek filter to a prefiltered dataset with fish and tag
#' # attributes
#' second_filter_lotek(filter_fish_detects)
second_filter_lotek <- function(fish_file){
  filtered <- fish_file
  filtered <- dplyr::group_by(.data = filtered, Tag_Hex)
  filtered <- dplyr::arrange(.data = filtered, Tag_Hex, DateTime_Local)
  filtered$td = difftime(filtered$DateTime_Local,
                         dplyr::lag(filtered$DateTime_Local), #compare the time difference between each hit
                         units = "secs")
  det_count <- dplyr::summarise(filtered, det_count = dplyr::n())
  filtered <- dplyr::left_join(filtered, det_count, by = "Tag_Hex")
  filtered$multipath <- ifelse(filtered$td > 0.3 | is.na(filtered$td), FALSE, TRUE) #identify any remaining multipath
  filtered$multipath <- ifelse(filtered$multipath == TRUE &
                                 dplyr::lead(filtered$multipath == FALSE),
                               FALSE,
                               filtered$multipath)
  filtered <- filtered[filtered$det_count > 3 & filtered$multipath == FALSE,] #first filter out any final multipath and any tags with <3 detects
  filtered$td = difftime(filtered$DateTime_Local,
                         dplyr::lag(filtered$DateTime_Local), # recalculate time diff
                         units = "secs")
  # Calculating for every 4 hits
  filtered$tdiff <- as.numeric(filtered$td)/round(as.numeric(filtered$td)/as.numeric(filtered$tag_pulse_rate_interval_nominal))
  filtered$tdiff <- ifelse(is.infinite(filtered$tdiff),NA,filtered$tdiff)
  filtered$tdiff <- round(filtered$tdiff*100)/100
  filtered$tdiff <- ifelse(is.na(filtered$tdiff),dplyr::lead(filtered$tdiff),filtered$tdiff)
  filtered$cs <- difftime(dplyr::lead(filtered$DateTime_Local,3),
                          filtered$DateTime_Local,
                          units = "secs")
  filtered$pr <- as.numeric(filtered$tag_pulse_rate_interval_nominal)
  filtered <- filtered[filtered$cs < filtered$pr*16.6 | is.na(filtered$cs),]

  filtered$v1 = abs(filtered$pr-filtered$tdiff) < filtered$pr*0.20
  filtered$v2 = abs(filtered$pr-dplyr::lead(filtered$tdiff)) < filtered$pr*0.20
  filtered$v3 = abs(filtered$pr-dplyr::lead(filtered$tdiff,2)) < filtered$pr*0.20
  filtered$sd_roll_check = rolling_sd_3(as.numeric(filtered$pr)) < 0.025
  filtered$sd_roll_check = ifelse(is.na(filtered$sd_roll_check),
                                  TRUE,
                                  filtered$sd_roll_check)
  filtered <- filtered[filtered$v1 == TRUE &
                         filtered$v2 == TRUE &
                         filtered$v3 == TRUE &
                         filtered$sd_roll_check == TRUE,]
  filtered <- dplyr::select(filtered, -c(det_count))
  det_count <- dplyr::summarise(filtered, det_count = dplyr::n())
  filtered <- dplyr::left_join(filtered, det_count, by = "Tag_Hex")
  filtered
}
