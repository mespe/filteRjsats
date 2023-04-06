#' Get Receiver Type
#'
#' This function takes a list of file paths to raw acoustic receiver detection
#' files supplied by the user and identifies the type of acoustic receiver
#' technology (Lotek, ATS, Teknologic) used to generate the file.
#'
#' @param file_list A vector of raw acoustic detection file paths
#' @returns A dataframe containing the file path, file name, and technology type
#' @export
#' @examples
#' # get a list of files
#' files <- system.file("extdata", package = "filteRjsats")|> list.files()
#'
#' # get the receiver type
#' get_receiver_type(files)
get_receiver_type <- function(file_list){
  receiver_type_list <- data.frame(file_name = rep(NA, length(file_list)),
                                   Name = rep(NA, length(file_list)),
                                   file_type = rep(NA, length(file_list)))
  for (i in 1:length(file_list)){
    file_name <- file_list[i]
    receiver_type_list$file_name[i] <- file_name
    receiver_type_list$Name[i] <- Filename <- stringr::str_split(file_name, pattern = '\\.')[[1]][1]
    receiver_type_list$file_type[i] <- ifelse(stringr::str_detect(file_name,'L'),
                                              "Lotek",
                                              ifelse(stringr::str_detect(file_name,'.SUM'),
                                                     "Teknologic",
                                                     "ATS"))
  }
  receiver_type_list
}
