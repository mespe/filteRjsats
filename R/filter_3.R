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
#' @return A dataframe which has been filtered to remove false positives
#' @export
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

#' Two Hit Filter for Teknologic Detections
#'
#' This function takes a Teknologic detection dataframe generated from the
#' add_fish() function and filters it a second time to remove any remaining
#' multipath detections, and then check the remaining detections by comparing
#' the time between each detection to ensure it is less 4x the stated pulse rate
#' interval. It additionally checks that all detections have a frequency
#' between 390 and 445 kHz and that the frequency of all detections are within
#' 55kHz of each other. Called by second_filter_2h4h().
#'
#'
#' @param fish_file a dataframe of detections retrieved from add_fish()
#' @return A dataframe which has been filtered to remove false positives
#' @export
second_filter_tekno <- function(fish_file){
  filtered <- fish_file
  filtered$pr_nom <- as.numeric(filtered$tag_pulse_rate_interval_nominal)
  filtered <- dplyr::group_by(.data = filtered, Tag_Hex)
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
  filtered <- filtered[filtered$td > 0.8*filtered$pr_nom|is.na(filtered$td),] # Remove short PRIs as false positives
  filtered$td = difftime(filtered$DateTime_Local,
                         dplyr::lag(filtered$DateTime_Local), # recalculate time diff
                         units = "secs")
  # Calculating for every two hits
  filtered$tdiff = as.numeric(filtered$td)/round(as.numeric(filtered$td)/as.numeric(filtered$pr_nom))
  filtered$tdiff = ifelse(is.infinite(filtered$tdiff),NA,filtered$tdiff)
  filtered$tdiff = round(filtered$tdiff*100)/100
  filtered$tdiff = ifelse(is.na(filtered$tdiff),
                          dplyr::lead(filtered$tdiff),
                          filtered$tdiff)
  filtered$FreqDiff = abs(filtered$Freq - dplyr::lead(filtered$Freq))
  filtered$freq_check = ifelse(filtered$FreqDiff < 55|
                                 is.na(filtered$FreqDiff),
                               TRUE,
                               FALSE)
  filtered$pr_check = ifelse(filtered$td < filtered$pr_nom*4 |
                               is.na(filtered$td),
                             TRUE,
                             FALSE)
  filtered$pr_check = ifelse(filtered$pr_check == FALSE &
                               dplyr::lead(filtered$td) < filtered$pr_nom*4,
                             TRUE,
                             filtered$pr_check)
  filtered <- filtered[filtered$pr_check == TRUE &
                         filtered$freq_check == TRUE,]
  filtered <- filtered[abs(as.numeric(filtered$pr_nom)-as.numeric(filtered$tdiff)) < (as.numeric(filtered$pr_nom)*0.1) &
                         filtered$Freq > 390.00 &
                         filtered$Freq < 445.00,]
  filtered <- dplyr::select(filtered, -c(det_count))
  det_count <- dplyr::summarise(filtered, det_count = dplyr::n())
  filtered <- dplyr::left_join(filtered, det_count, by = "Tag_Hex")

  filtered
}

#' Two Hit Filter for ATS Detections
#'
#' This function takes an ATS detection dataframe generated from the
#' add_fish() function and filters it a second time to remove any remaining
#' multipath detections, and then check the remaining detections by comparing
#' the time between each detection to ensure it is less 4x the stated pulse rate
#' interval. It additionally checks that all detections have a frequency
#' between 416.3 and 418.75 kHz and that the frequency of all detections are within
#' 0.505kHz of each other. Called by second_filter_2h4h().
#'
#'
#' @param fish_file a dataframe of detections retrieved from add_fish()
#' @return A dataframe which has been filtered to remove false positives
#' @export
second_filter_ats <- function(fish_file){
  filtered <- fish_file
  filtered$pr_nom <- as.numeric(filtered$tag_pulse_rate_interval_nominal)
  filtered <- dplyr::group_by(.data = filtered, Tag_Hex)
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
  filtered <- filtered[filtered$td > 0.8*filtered$pr_nom|is.na(filtered$td),] # Remove short PRIs as false positives
  filtered$td = difftime(filtered$DateTime_Local,
                         dplyr::lag(filtered$DateTime_Local), # recalculate time diff
                         units = "secs")
  # Calculating for every two hits
  filtered$tdiff = as.numeric(filtered$td)/round(as.numeric(filtered$td)/as.numeric(filtered$pr_nom))
  filtered$tdiff = ifelse(is.infinite(filtered$tdiff),NA,filtered$tdiff)
  filtered$tdiff = round(filtered$tdiff*100)/100
  filtered$tdiff = ifelse(is.na(filtered$tdiff),
                          dplyr::lead(filtered$tdiff),
                          filtered$tdiff)
  filtered$FreqDiff = abs(filtered$Freq - dplyr::lead(filtered$Freq))
  filtered$freq_check = ifelse(filtered$FreqDiff < 0.505|
                                 is.na(filtered$FreqDiff),
                               TRUE,
                               FALSE)
  filtered$pr_check = ifelse(filtered$td < filtered$pr_nom*4 |
                               is.na(filtered$td),
                             TRUE,
                             FALSE)
  filtered$pr_check = ifelse(filtered$pr_check == FALSE &
                               dplyr::lead(filtered$td) < filtered$pr_nom*4,
                             TRUE,
                             filtered$pr_check)
  filtered <- filtered[filtered$pr_check == TRUE &
                         filtered$freq_check == TRUE,]
  filtered <- filtered[abs(as.numeric(filtered$pr_nom)-as.numeric(filtered$tdiff)) < (as.numeric(filtered$pr_nom)*0.1) &
                         filtered$Freq > 416.30 &
                         filtered$Freq < 418.75,]
  filtered <- dplyr::select(filtered, -c(det_count))
  det_count <- dplyr::summarise(filtered, det_count = dplyr::n())
  filtered <- dplyr::left_join(filtered, det_count, by = "Tag_Hex")
  filtered
}

#' Four Hit Filter for Tekno Detections
#'
#' This function takes an Tekno detection dataframe generated from the
#' add_fish() function and filters it a second time to remove any remaining
#' multipath detections, and then check the remaining detections by comparing the time
#' between detections, for a rolling window of 4 detections to ensure it is less
#' 16.6x the stated pulse rate interval. It additionally checks that all detections have a frequency
#' between 390 and 445 kHz and that the frequency of all detections are within
#' 55kHz of each other. Called by second_filter_4h().
#'
#'
#' @param fish_file a dataframe of detections retrieved from add_fish()
#' @return A dataframe which has been filtered to remove false positives
#' @export
second_filter_tekno_4h <- function(fish_file){
  filtered <- fish_file
  filtered$pr_nom <- as.numeric(filtered$tag_pulse_rate_interval_nominal)
  filtered <- dplyr::group_by(.data = filtered, Tag_Hex)
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
  filtered <- filtered[filtered$td > 0.8*filtered$pr_nom|is.na(filtered$td),] # Remove short PRIs as false positives
  filtered$td = difftime(filtered$DateTime_Local,
                         dplyr::lag(filtered$DateTime_Local), # recalculate time diff
                         units = "secs")
  # Calculating for every four hits (updated due to excessive noise in South Delta)
  filtered$tdiff <- as.numeric(filtered$td)/round(as.numeric(filtered$td)/as.numeric(filtered$tag_pulse_rate_interval_nominal))
  filtered$tdiff <- ifelse(is.infinite(filtered$tdiff),NA,filtered$tdiff)
  filtered$tdiff <- round(filtered$tdiff*100)/100
  filtered$tdiff <- ifelse(is.na(filtered$tdiff),dplyr::lead(filtered$tdiff),filtered$tdiff)
  filtered$cs <- difftime(dplyr::lead(filtered$DateTime_Local,3),
                          filtered$DateTime_Local,
                          units = "secs")
  filtered$pr <- as.numeric(filtered$tag_pulse_rate_interval_nominal)
  filtered <- filtered[filtered$cs < filtered$pr_nom*16.6 | is.na(filtered$cs),]
  filtered$v1 = abs(filtered$pr_nom-filtered$tdiff) < filtered$pr_nom*0.20
  filtered$v2 = abs(filtered$pr_nom-dplyr::lead(filtered$tdiff)) < filtered$pr_nom*0.20
  filtered$v3 = abs(filtered$pr_nom-dplyr::lead(filtered$tdiff,2)) < filtered$pr_nom*0.20
  filtered$sd_roll_check = rolling_sd_3(as.numeric(filtered$pr_nom)) < 0.025
  filtered$sd_roll_check = ifelse(is.na(filtered$sd_roll_check),
                                  TRUE,
                                  filtered$sd_roll_check)
  filtered$FreqDiff = abs(filtered$Freq - dplyr::lead(filtered$Freq))
  filtered$freq_check = ifelse(filtered$FreqDiff < 55|
                                 is.na(filtered$FreqDiff),
                               TRUE,
                               FALSE)
  filtered$pr_check = ifelse(filtered$td < filtered$pr_nom*4 |
                               is.na(filtered$td),
                             TRUE,
                             FALSE)
  filtered$pr_check = ifelse(filtered$pr_check == FALSE &
                               dplyr::lead(filtered$td) < filtered$pr_nom*4,
                             TRUE,
                             filtered$pr_check)
  filtered <- filtered[filtered$pr_check == TRUE &
                         filtered$freq_check == TRUE &
                         filtered$v1 == TRUE &
                         filtered$v2 == TRUE &
                         filtered$v3 == TRUE &
                         filtered$sd_roll_check == TRUE,]
  filtered <- filtered[abs(as.numeric(filtered$pr_nom)-as.numeric(filtered$tdiff)) < (as.numeric(filtered$pr_nom)*0.1) &
                         filtered$Freq > 390.00 &
                         filtered$Freq < 445.00,]
  filtered <- dplyr::select(filtered, -c(det_count))
  det_count <- dplyr::summarise(filtered, det_count = dplyr::n())
  filtered <- dplyr::left_join(filtered, det_count, by = "Tag_Hex")

  filtered
}

#' Four Hit Filter for ATS Detections
#'
#' This function takes an ATS detection dataframe generated from the
#' add_fish() function and filters it a second time to remove any remaining
#' multipath detections, and then check the remaining detections by comparing the time
#' between detections, for a rolling window of 4 detections to ensure it is less
#' 16.6x the stated pulse rate interval. It additionally checks that all
#' detections within the window occur within 20% of the pulse rate interval of
#' the other detections and that the standard deviation of pulse rate intervals
#' is less than 0.025. It additionally checks that all detections have a frequency
#' between 416.3 and 418.75 kHz and that the frequency of all detections are within
#' 0.505kHz of each other. Called by second_filter().
#'
#'
#' @param fish_file a dataframe of detections retrieved from add_fish()
#' @return A dataframe which has been filtered to remove false positives
#' @export
second_filter_ats_4h <- function(fish_file){
  filtered <- fish_file
  filtered$pr_nom <- as.numeric(filtered$tag_pulse_rate_interval_nominal)
  filtered <- dplyr::group_by(.data = filtered, Tag_Hex)
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
  filtered <- filtered[filtered$td > 0.8*filtered$pr_nom|is.na(filtered$td),] # Remove short PRIs as false positives
  filtered$td = difftime(filtered$DateTime_Local,
                         dplyr::lag(filtered$DateTime_Local), # recalculate time diff
                         units = "secs")
  # Calculating for every four hits
  filtered$tdiff = as.numeric(filtered$td)/round(as.numeric(filtered$td)/as.numeric(filtered$pr_nom))
  filtered$tdiff = ifelse(is.infinite(filtered$tdiff),NA,filtered$tdiff)
  filtered$tdiff = round(filtered$tdiff*100)/100
  filtered$tdiff = ifelse(is.na(filtered$tdiff),
                          dplyr::lead(filtered$tdiff),
                          filtered$tdiff)
  filtered$cs <- difftime(dplyr::lead(filtered$DateTime_Local,3),
                          filtered$DateTime_Local,
                          units = "secs")
  filtered$pr <- as.numeric(filtered$tag_pulse_rate_interval_nominal)
  filtered <- filtered[filtered$cs < filtered$pr_nom*16.6 | is.na(filtered$cs),]
  filtered$v1 = abs(filtered$pr_nom-filtered$tdiff) < filtered$pr_nom*0.20
  filtered$v2 = abs(filtered$pr_nom-dplyr::lead(filtered$tdiff)) < filtered$pr_nom*0.20
  filtered$v3 = abs(filtered$pr_nom-dplyr::lead(filtered$tdiff,2)) < filtered$pr_nom*0.20
  filtered$sd_roll_check = rolling_sd_3(as.numeric(filtered$pr_nom)) < 0.025
  filtered$sd_roll_check = ifelse(is.na(filtered$sd_roll_check),
                                  TRUE,
                                  filtered$sd_roll_check)
  filtered$FreqDiff = abs(filtered$Freq - dplyr::lead(filtered$Freq))
  filtered$freq_check = ifelse(filtered$FreqDiff < 0.505|
                                 is.na(filtered$FreqDiff),
                               TRUE,
                               FALSE)
  filtered$pr_check = ifelse(filtered$td < filtered$pr_nom*4 |
                               is.na(filtered$td),
                             TRUE,
                             FALSE)
  filtered$pr_check = ifelse(filtered$pr_check == FALSE &
                               dplyr::lead(filtered$td) < filtered$pr_nom*4,
                             TRUE,
                             filtered$pr_check)
  filtered <- filtered[filtered$pr_check == TRUE &
                         filtered$freq_check == TRUE &
                         filtered$v1 == TRUE &
                         filtered$v2 == TRUE &
                         filtered$v3 == TRUE &
                         filtered$sd_roll_check == TRUE,]
  filtered <- filtered[abs(as.numeric(filtered$pr_nom)-as.numeric(filtered$tdiff)) < (as.numeric(filtered$pr_nom)*0.1) &
                         filtered$Freq > 416.30 &
                         filtered$Freq < 418.75,]
  filtered <- dplyr::select(filtered, -c(det_count))
  det_count <- dplyr::summarise(filtered, det_count = dplyr::n())
  filtered <- dplyr::left_join(filtered, det_count, by = "Tag_Hex")
  filtered
}

#' Two or Four Hit Filter for Any Detection File
#'
#' This function takes any acoustic receiver detection dataframe generated from
#' the add_fish() function and filters it a second time to remove any remaining
#' multipath detections. Then depending on the type of receiver file applies an
#' algorithm to assess and filter all remaining detections based on 2 or 4 hits.
#'
#'
#' @param fish_file a dataframe of detections retrieved from add_fish()
#' @return A dataframe which has been filtered to remove false positives
#' @export
second_filter_2h4h <- function(fish_file){

  make = fish_file$Make[1]
  final_file = NULL
  if (make == "Lotek") final_file <- second_filter_lotek(fish_file)
  if (make == "Tekno") final_file <- second_filter_tekno(fish_file)
  if (make == "ATS")   final_file <- second_filter_ats(fish_file)
  if (is.null(final_file)) errorCondition("Receiver make should be one of Lotek, Tekno, or ATS")

  message(paste0("Number of Valid Tag IDs: ", length(unique(final_file$Tag_Hex))))
  message(paste0("Number of Valid Detections: ", length(final_file$DateTime_Local)))

  spur <- compare_df(fish_file, final_file)

  message(paste0("Number of Tags with Spurious Detections: ", length(unique(spur$Tag_Hex))))
  message(paste0("Tags with Only Spurious Detections: "), spur$TagHex[which(!(unique(spur$Tag_Hex) %in% (unique(final_file$Tag_Hex))))])
  message(paste0("Number of Spurious Detections: ", length(spur$DateTime_Local)))

  final_file
}

#' Four Hit Filter for Any Detection File
#'
#' This function takes any acoustic receiver detection dataframe generated from
#' the add_fish() function and filters it a second time to remove any remaining
#' multipath detections. Then applies an algorithm to assess and filter all
#' remaining detections based on a four hit filter.
#'
#'
#' @param fish_file a dataframe of detections retrieved from add_fish()
#' @return A dataframe which has been filtered to remove false positives
#' @export
second_filter_4h <- function(fish_file){

  make = fish_file$Make[1]
  final_file = NULL
  if (make == "Lotek") final_file <- second_filter_lotek(fish_file)
  if (make == "Tekno") final_file <- second_filter_tekno(fish_file)
  if (make == "ATS")   final_file <- second_filter_ats(fish_file)
  if (is.null(final_file)) errorCondition("Receiver make should be one of Lotek, Tekno, or ATS")

  message(paste0("Number of Valid Tag IDs: ", length(unique(final_file$Tag_Hex))))
  message(paste0("Number of Valid Detections: ", length(final_file$DateTime_Local)))

  spur <- compare_df(fish_file, final_file)

  message(paste0("Number of Tags with Spurious Detections: ", length(unique(spur$Tag_Hex))))
  message(paste0("Tags with Only Spurious Detections: "), spur$TagHex[which(!(unique(spur$Tag_Hex) %in% (unique(final_file$Tag_Hex))))])
  message(paste0("Number of Spurious Detections: ", length(spur$DateTime_Local)))

  final_file
}
