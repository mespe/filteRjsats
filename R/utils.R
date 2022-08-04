compare_df  <- function(a1,a2){
  a3 <- dplyr::select(a1, Tag_Hex, DateTime_Local)
  a4 <- dplyr::select(a2, Tag_Hex, DateTime_Local)
  a1.vec <- apply(a3, 1, paste, collapse = "")
  a2.vec <- apply(a4, 1, paste, collapse = "")
  a1.without.a2.rows <- a1[!a1.vec %in% a2.vec,]
  return(a1.without.a2.rows)
}

rolling_sd_3 <- function(x){
  mean = (x + lead(x) + lead(x,2))/3
  sos = (x-mean)^2+ (lead(x)-mean)^2 + (lead(x,2)-mean)^2
  sd = sqrt(sos/2)

  return(sd)
}
