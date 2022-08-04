join_rcvr_data <- function(final_file, rcvr_data){
  out <- dplyr::left_join(final_file,rcvr_data,by = "ReceiverSN")
  out <- out[out$DateTime_Local > out$receiver_start &
               out$DateTime_Local < out$receiver_end,]
  out
}
