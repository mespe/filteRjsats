#' Register Parallel
#'
#' This function registers the various necessary packages on your machine for
#'for the parallel processing of files.
#'
#' @param files unfiltered detection dataframe
#' @return Your machine is now ready to use parallel processing for this package
#' @export
register_parallel <- function(files){
  cl <- snow::makeCluster(parallel::detectCores()-2, "SOCK")
  doSNOW::registerDoSNOW(cl)
  snow::clusterEvalQ(cl, {
    library(lubridate)
    library(tidyr)
    library(stringr)
    library(broman)
    library(dplyr)})
  nrun <- length(files)
  pb <- txtProgressBar(max=nrun, style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
}
