#' Compare two Detection Dataframes
#'
#' This function compares two detection dataframes from filtered acoustic
#' telemetry detections and identifies the filtered out detections. The first
#' input is the original dataframe, the second is the filtered dataframe. Then
#' it creates a dataframe of detections which are missing from the filtered
#' dataframe.
#'
#' @param a1 unfiltered detection dataframe
#' @param a2 filtered detection dataframe
#' @return A dataframe of detections from the original dataframe missing from
#' the filtered dataframe. Spurious detections.
#' @export
#' @examples
#' # Compare detections before and after filtering
#' compare_detects(raw_ats[1:1000,], filter_fish_detects[1:1000,])
compare_detects  <- function(a1,a2){
  a3 <- dplyr::select(a1, Tag_Hex, DateTime_Local)
  a4 <- dplyr::select(a2, Tag_Hex, DateTime_Local)
  a1.vec <- apply(a3, 1, paste, collapse = "")
  a2.vec <- apply(a4, 1, paste, collapse = "")
  a1.without.a2.rows <- a1[!a1.vec %in% a2.vec,]
  return(a1.without.a2.rows)
}

#' Calculate Standard Deviation Over a Rolling Window of 3
#'
#' This function is called within the third filter, and used to calculate a
#' rolling standard deviation with a window including the 2 leading values.
#'
#' @param x an indexed position of a value in a vector for which the rolling standard deviation is needed
#' @returns the standard deviation of x, and the two leading values
#' @export
#' @examples
#' # Calculate rolling Standard Deviation
#' set.seed(1234)
#' x <- rnorm(n = 100, mean = 10, sd = 1)
#' rolling_sd_3(x)
rolling_sd_3 <- function(x){
  mean = (x + dplyr::lead(x) + dplyr::lead(x,2))/3
  sos = (x-mean)^2+ (dplyr::lead(x)-mean)^2 + (dplyr::lead(x,2)-mean)^2
  sd = sqrt(sos/2)
  return(sd)
}

#' Add in Global Variables
#'
#' Sets all global variables to remove warnings in package build
#'
#' @name Set_GVs
utils::globalVariables(c('B2', 'BitPer', 'DateTime_Local', 'Freq', 'Make',
                         'ReceiverSN','RefTag', 'SigStr', 'Tag_Decimal',
                         'Tag_Hex', 'Temp', 'Thres','Tilt', 'Volt',
                         'fish_release_date', 'lines', 'multipath',
                         'rcvr_fields', 'read.csv', 'read.delim',
                         'receiver_beacon_id_hex', 'receiver_retrieve',
                         'receiver_serial_number', 'receiver_start',
                         'setTxtProgressBar', 'tag_life',
                         'tag_pulse_rate_interval_nominal', 'time_diff_lag',
                         'txtProgressBar', '%>%', 'receiver_end'))
