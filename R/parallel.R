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
