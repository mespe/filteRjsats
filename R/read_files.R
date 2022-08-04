#' @export
get_receiver_type <- function(file_list){
  receiver_type_list <- data.frame(file_name = rep(NA, length(file_list)),
                                   Name = rep(NA, length(file_list)),
                                   file_type = rep(NA, length(file_list)))
  for (i in 1:length(file_list)){
    file_name <- file_list[i]
    receiver_type_list$file_name[i] <- file_name
    receiver_type_list$Name <- Filename <- stringr::str_split(file_name, pattern = '\\.')[[1]][1]
    receiver_type_list$file_type[i] <- ifelse(stringr::str_detect(file_name,'L'),
                                              "Lotek",
                                              ifelse(stringr::str_detect(file_name,'.SUM'),
                                                     "Teknologic",
                                                     "ATS"))
  }
  receiver_type_list
}

#' @export
read_lotek <- function(path, file, timezone){
  LOT = read.csv(file.path(path,file),
                 colClasses = c(character(),
                                numeric(),
                                numeric(),
                                character(),
                                numeric()),
                 col.names = c("R","DT","FS","D","H","P"),
                 skip = 0)
  LOT$R <- as.numeric(gsub("WHS4K-","",LOT$R)) #Turn the file name into serial
  LOT$DT =  as.POSIXct(LOT$DT, #Convert serial DateTime to real DT
                       tz = timezone,
                       origin = "1899-12-30")
  LOT$FS = lubridate::seconds(LOT$FS) #Convert fractional seconds from number to seconds
  LOT$DT = LOT$DT+LOT$FS #Add them together
  LOT$Filename = stringr::str_split(file, pattern = '\\.')[[1]][1]
  LOT$Make = "Lotek"
  LOT
}

#' @export
read_tekno <- function(path, file, timezone){

  TEK <- read.csv(file.path(path,file),
                  colClasses = c(character(),character(),character(),
                                 character(),numeric(),numeric(),
                                 numeric(), numeric(), numeric(),
                                 numeric(), numeric(), numeric(),
                                 numeric(), logical()),
                  skip = 8,
                  header = TRUE)

  TEK$H = as.character(substr(TEK$TagCode,4,7)) #Extract hex format of tag code
  TEK <- TEK[TEK$valid == 1,] # filter out "invalid" detections X = 0
  TEK$D = broman::hex2dec(TEK$H) # Convert hex to dec format
  TEK$DT = lubridate::mdy_hms(TEK$Date.Time, tz = timezone)
  TEK$Volt = TEK$vBatt
  TEK$Freq = 416.666 + (TEK$Freq/1000)
  TEK$Thres = TEK$Thresh
  TEK$SigStr = TEK$snr
  TEK$Make = "Tekno"
  TEK

}

#' @export
read_ats <- function(path, file, timezone){
  ATS <- data.frame(read.delim(file = file.path(path,file),
                               skip = 0))
  colnames(ATS) <- "lines"
  ATS$G72 = grepl("G72",ATS$lines)
  ATS$commas = lengths(regmatches(ATS$lines, gregexpr(",",ATS$lines)))
  ATS$valid = ifelse(ATS$G72 == TRUE & ATS$commas == 13, TRUE, FALSE)
  ATS <- ATS[ATS$valid == TRUE,]
  ATS <- tidyr::separate(data = ATS,
                         col = lines,
                         sep = ",",
                         into = c(NA,NA,NA,NA,'DateTime','TagCode',
                                  'Tilt', 'VBatt', 'Temp', 'Pressure',
                                  'SigStr', 'Bit_Period', 'Threshold'))
  ATS$DT_Check = grepl('\\.',ATS$DateTime)
  ATS <- ATS[ATS$DT_Check == TRUE,]
  ATS$DT = lubridate::mdy_hms(ATS$DateTime, tz = timezone)
  ATS$FullID = ATS$TagCode
  ATS$H = as.character(substr(ATS$TagCode,5,8)) #There's a space before the G
  ATS$D = broman::hex2dec(ATS$H)
  ATS$Tilt = as.numeric(ATS$Tilt)
  ATS$Volt = as.numeric(ATS$VBatt)
  ATS$Temp = as.numeric(ATS$Temp)
  ATS$SigStr = as.numeric(ATS$SigStr)
  ATS$BitPer = ATS$Bit_Period
  ATS <- tidyr::separate(data = ATS,
                         col = BitPer,
                         into = c(NA,"B1","B2"),
                         sep = "\\s")
  ATS <- tidyr::separate(data = ATS,
                         col = B2,
                         into = c("B3","B4"),
                         sep = c("/",","))
  ATS$Freq = ifelse(is.na(ATS$B3),-999, 100000/(as.numeric(ATS$B1)+(as.numeric(ATS$B3)/31)))
  ATS$Thres = ifelse(is.na(ATS$B3), -999, as.numeric(ATS$Threshold))
  R <-  gsub("SR","",stringr::str_split(file, pattern = '\\.')[[1]][1])
  R <- stringr::str_split(R,pattern = '_')[[1]][1]
  ATS$R = R
  ATS$Make = "ATS"
  ATS
}

#' @export
read_jsats <- function(path, file, timezone){
  # file_name <- stringr::str_split(file, pattern = '\\.')[[1]][1]
  file_type <- ifelse(stringr::str_detect(file,'L'),
                      "Lotek",
                      ifelse(stringr::str_detect(file,'.SUM'),
                             "Teknologic",
                             "ATS"))
  if(!(file_type %in% c("Lotek","Teknologic","ATS"))) jsats_file <- data.frame()
  if(file_type == "Lotek") jsats_file <- read_lotek(path, file, timezone)
  if(file_type == "Teknologic") jsats_file <- read_tekno(path, file, timezone)
  if(file_type == "ATS") jsats_file <- read_ats(path, file, timezone)
  jsats_file
}
