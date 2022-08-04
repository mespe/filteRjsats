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

  filtered$v1 = abs(filtered$pr-filtered$tdiff) < pr*0.20
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

second_filter <- function(fish_file){

  if (fish_file$Make[1] == "Lotek") final_file <- second_filter_lotek(fish_file)
  if (fish_file$Make[1] == "Tekno") final_file <- second_filter_tekno(fish_file)
  if (fish_file$Make[1] == "ATS") final_file <- second_filter_ats(fish_file)

  message(paste0("Number of Valid Tag IDs: ", length(unique(final_file$Tag_Hex))))
  message(paste0("Number of Valid Detections: ", length(final_file$DateTime_Local)))

  spur <- compare_df(fish_file, final_file)

  message(paste0("Number of Spurious Tag IDs: ", length(unique(spur$Tag_Hex))))
  message(paste0("Number of Spurious Detections: ", length(spur$DateTime_Local)))

  final_file
}
