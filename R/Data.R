#' Example raw data from ats receiver after being read in by read_jsats
#'
#' A dataframe of ATS acoustic receiver data processed by `read_jsats()`
#'
#' @format ## `raw_ats`
#' a dataframe with 261,648 rows and 11 columns:
#' \describe{
#'   \item{ReceiverSN}{the serial number of the acoustic receiver which heard the
#'    detection}
#'   \item{Make}{The Manufacturer of the acoustic reciever}
#'   \item{DateTime_Local}{The local date and time of the detection
#'    tz = "America/Los_Angeles"}
#'   \item{Tag_Decimal}{The decimal value of the tag ID code}
#'   \item{Tag_Hex}{The hexadecimal value of the tag ID code}
#'   \item{Tilt}{The tilt of the acoustic receiver from a vertical axis}
#'   \item{Volt}{The voltage of the on-board battery of the receiver}
#'   \item{Temp}{The water temperature outside of the receiver}
#'   \item{SigStr}{The strength of the acoustic signal in dB}
#'   \item{Freq}{The frequency of the acoustic signal in kHz}
#'   \item{Thres}{The relative amount of external background noise,
#'    signal threshold}
#' }
"raw_ats"


#' Example reference tags
#'
#' A vector of example reference tag codes
#'
#' @format A vector of example reference tag codes
"reftags"

#' A List of Important Receiver Related Fields from ERDAPP
#'
#' A vector of acoustic receiver metadata fields which are needed, at a minimum,
#' to run the join_rcvr_data function
#'
#' @format A vector of potential receiver metadata fields which the user may review
"rcvr_fields"

#' Previously filtered detection data
#'
#' An example dataset of real acoustic telemetry detections of fish at several
#' receivers within the California Central Valley from 2021. These detections
#' have already been been processed to remove false positives using the various
#' filtering functions in this package and in companion package `filteRjsats`.
#'
#' @format ## `filtered_detections`
#' A data frame with 100,000 rows and 3 columns:
#' \describe{
#'   \item{serial}{The serial number of the detecting receiver}
#'   \item{local_time}{the local time of the detection (tz = America/Los_Angeles)}
#'   \item{tag_id}{The hexadecimal acoustic tag ID code}
#' }
#' @source Data collected by the California Department of Water Resources 2021
"filtered_detections"

#' Receiver Data
#'
#' An example dataset of real acoustic telemetry receivers within the California
#' Central Valley in 2021. These receivers are only those which match the
#' serial numbers in companion dataset `filtered_detections`. This data is
#' formatted to match the California Fish Tracking receiver metadata found here:
#' https://oceanview.pfeg.noaa.gov/CalFishTrack/.
#'
#' @format ## `receivers`
#' A data frame with 52 rows and 10 columns:
#' \describe{
#'   \item{dep_id}{A unique id is created for each receiver deployment}
#'   \item{receiver_make}{The brand of the acoustic receiver}
#'   \item{receiver_serial_number}{The serial number of the acoustic receiver}
#'   \item{latitude}{The decimal degree latitude (WGS1984) of the acoustic
#'   receiver at deployment}
#'   \item{longitude}{The decimal degree longitude (WGS1984) of the acoustic
#'   receiver at deployment}
#'   \item{receiver_location}{The site name of an individual receiver, often
#'   more than one `receiver_location` is found at a `receiver_general_location`}
#'   \item{receiver_general_location}{The more general geographic name of the
#'   location of the receiver}
#'   \item{receiver_river_km}{The number of river kilometers the receiver is
#'   from the Golden Gate Bridge}
#'   \item{receiver_start}{The start time of the reciever (generally when it
#'   was deployed)}
#'   \item{receiver_end}{The end time of the receiver (generally when it was
#'   retrieved)}
#' }
#' @source <https://oceanview.pfeg.noaa.gov/CalFishTrack/pageRealtime_download.html>
"receivers"

#' Fish Data
#'
#' An example dataset of real fish tagged with acoustic telemetry tags and
#' released within the California Central Valley in 2021 and 2022.
#'
#' @format ## `fish`
#' A data frame with 17,227 rows and 10 columns:
#' \describe{
#'   \item{fish_type}{Generally a strain, run, and species of fish (e.g.
#'   Nimbus Fall Chinook = Fall-run Chinook Salmon from Nimbus Hatchery)}
#'   \item{TagCode}{The hexadecimal code of the implanted acoustic tag}
#'   \item{Release_Date}{The release date and time of the fish}
#'   \item{release_location}{The coded name of the release site}
#'   \item{length}{The length of the fish in millimeters}
#'   \item{weight}{The weight of the fish in grams}
#'   \item{tag_weight}{The weight of the implanted acoustic tag}
#'   \item{tag_model}{The model number of the implanted acoustic tag}
#'   \item{PRI}{The pulse rate interval (time between transmissions) of the
#'   implanted tag, as reported by the manufacturer}
#'   \item{TagLife}{The expected number of days the tag should continue to
#'   transmit, as reported by the manufacturer}
#' }
#' @source <https://oceanview.pfeg.noaa.gov/CalFishTrack/pageRealtime_download.html>
"fish"

#' Example Prefiltered Detection Dataframe
#'
#' A dataframe of detections which has gone through the `prefilter()` function
#'
#' @format ## `prefiltered_detects`
#' A dataframe with 209,149 rows and 12 columns
#' \describe{
#'   \item{ReceiverSN}{the serial number of the acoustic receiver which heard the
#'    detection}
#'   \item{Make}{The Manufacturer of the acoustic reciever}
#'   \item{DateTime_Local}{The local date and time of the detection
#'    tz = "America/Los_Angeles"}
#'   \item{Tag_Decimal}{The decimal value of the tag ID code}
#'   \item{Tag_Hex}{The hexadecimal value of the tag ID code}
#'   \item{Tilt}{The tilt of the acoustic receiver from a vertical axis}
#'   \item{Volt}{The voltage of the on-board battery of the receiver}
#'   \item{Temp}{The water temperature outside of the receiver}
#'   \item{SigStr}{The strength of the acoustic signal in dB}
#'   \item{Freq}{The frequency of the acoustic signal in kHz}
#'   \item{Thres}{The relative amount of external background noise,
#'    signal threshold}
#'   \item{CheckMBP}{A calculated field from the first filter checking the
#'    time between acoustic transmissions from the same tag was >0.3secs}
#' }
"prefiltered_detects"

#' Example fish detection data
#'
#' Example tag detection data from CalFishTrack representing fish released in
#' 2021.
#'
#' @format ## `fish_detects`
#' A dataframe with 10,000 rows and 3 columns
#' \describe{
#'   \item{ReceiverSN}{the serial number of the acoustic receiver which heard
#'   the detection}
#'   \item{DateTime_Local}{The local date and time of the detection
#'    tz = "America/Los_Angeles"}
#'   \item{Tag_Code}{The hexadecimal value of the acoustic tag ID code}
#' }
"fish_detects"

#' Example fish detection data which has been prefiltered and has fish data
#'
#' Example tag detection data representing a single file which has been
#' processed using the `prefilter()` and `add_fish()` functions.
#'
#' @format ## `filter_fish_detects`
#' A dataframe with 5,000 rows and 39 columns
#' \describe{
#'   \item{ReceiverSN}{the serial number of the acoustic receiver which heard the
#'    detection}
#'   \item{Make}{The Manufacturer of the acoustic reciever}
#'   \item{DateTime_Local}{The local date and time of the detection
#'    tz = "America/Los_Angeles"}
#'   \item{Tag_Decimal}{The decimal value of the tag ID code}
#'   \item{Tag_Hex}{The hexadecimal value of the tag ID code}
#'   \item{Tilt}{The tilt of the acoustic receiver from a vertical axis}
#'   \item{Volt}{The voltage of the on-board battery of the receiver}
#'   \item{Temp}{The water temperature outside of the receiver}
#'   \item{SigStr}{The strength of the acoustic signal in dB}
#'   \item{Freq}{The frequency of the acoustic signal in kHz}
#'   \item{Thres}{The relative amount of external background noise,
#'    signal threshold}
#'   \item{CheckMBP}{A calculated field from the first filter checking the
#'    time between acoustic transmissions from the same tag was >0.3secs}
#'   \item{TagInFile}{A calculated field from the add_fish filter which
#'    queries whether the tag code of the detection is associated with a fish.}
#'   \item{fish_id}{Identifies the fish that was tagged. It is unique in that no
#'    two fish have the same FishID. Format is 2 or more letters that describe
#'    the type of the fish (e.g. WR for WinterRun), followed by the year (YYYY),
#'    followed by a dash, then a sequential three digit number (e.g. 001 to 999)}
#'   \item{study_id}{Identifies a group of tagged fish that belong to a study.
#'    Format is text description of place followed by year (YYYY). A single
#'    StudyID can have fish release on multiple days within a year and/or at
#'    multiple locations within a year.}
#'   \item{fish_type}{Describes the fish tagged. Generally a part that describes
#'    where it came from and a part that refers to the common name.}
#'   \item{fish_origin}{Describes where the fish is from or where it was
#'    collected. Example: Hatchery, Natural, Sacramento River.}
#'   \item{fish_date_tagged}{The date and time that indicates the time the tag
#'    was activated and implanted into the fish. Tags are usually activated
#'    several minutes before implanting into fish in PST.}
#'   \item{fish_release_date}{The date and time of release in PST.}
#'   \item{tag_id_hex}{The hexadecimal form for the tags code. This is not
#'    unique in that two fish can have the same Hex Tag Code. This is usually
#'    from the same tag code being used in different years. This is why all
#'    queries must be based on Fish ID.}
#'   \item{tag_id_decimal}{The decimal form for the tags code. This is not
#'    unique in that two fish can have the same Decimal Tag Code. This is
#'    usually from the same tag code being used in different years. This is why
#'    all queries must be based on Fish ID.}
#'   \item{tag_weight}{weight of tag in air}
#'   \item{tag_model}{The model of the tag.}
#'   \item{tag_pulse_rate_interval_nominal}{The nominal (aka approximate) pulse
#'    rate interval. This is how often the tag transmits its code signal.}
#'   \item{tag_warranty_life}{The minimum number of days a tag is expected to
#'    transmit its code. Generally tags transmit for at least 1.5x the warranty
#'    life.}
#'   \item{fish_length_type}{The way the fish was measured. Fork length = FL,
#'    total length = TL, standard length = SL. If unknown = NA.}
#'   \item{fish_length}{Measured fish length in millimeters}
#'   \item{fish_weight}{Measured fish weight in grams (in air)}
#'   \item{release_location}{The name of the place that the fish was released.}
#'   \item{release_latitude}{The latitude of the release location.}
#'   \item{release_longitude}{The longitude of the release location.}
#'   \item{release_river_km}{The river km of the release location.
#'    The Golden Gate Bridge = rkm 0.0. Values increase the further upstream.}
#'   \item{email}{the email address of the point of contact that grants approval
#'    for using the data from each fish.}
#'   \item{release_rkm}{The river km of the release location.
#'    The Golden Gate Bridge = rkm 0.0. Values increase the further upstream.}
#'   \item{tag_life}{The minimum number of days a tag is expected to
#'    transmit its code. Generally tags transmit for at least 1.5x the warranty
#'    life.}
#'   \item{length}{Measured fish length in millimeters}
#'   \item{weight}{Measured fish weight in grams (in air)}
#'   \item{CheckDT}{A calculated field which checks whether the detection
#'    occurred after the release of the fish}
#'   \item{CheckBattLife}{A calculated field which checks whether the detection
#'    occurred before the tag battery is expected to expire (2x tag life)}
#' }
"filter_fish_detects"

#' Example fish data from CalFishTrack
#'
#' A dataframe of acoustically tagged fish downloaded from ERDDAP representing
#' fish released in 2021 and 2022.
#'
#' @format ## `cft_fish`
#' A dataframe with 17,227 rows and 25 columns
#' \describe{
#'   \item{fish_id}{Identifies the fish that was tagged. It is unique in that no
#'    two fish have the same FishID. Format is 2 or more letters that describe
#'    the type of the fish (e.g. WR for WinterRun), followed by the year (YYYY),
#'    followed by a dash, then a sequential three digit number (e.g. 001 to 999)}
#'   \item{study_id}{Identifies a group of tagged fish that belong to a study.
#'    Format is text description of place followed by year (YYYY). A single
#'    StudyID can have fish release on multiple days within a year and/or at
#'    multiple locations within a year.}
#'   \item{fish_type}{Describes the fish tagged. Generally a part that describes
#'    where it came from and a part that refers to the common name.}
#'   \item{fish_origin}{Describes where the fish is from or where it was
#'    collected. Example: Hatchery, Natural, Sacramento River.}
#'   \item{fish_date_tagged}{The date and time that indicates the time the tag
#'    was activated and implanted into the fish. Tags are usually activated
#'    several minutes before implanting into fish in PST.}
#'   \item{fish_release_date}{The date and time of release in PST.}
#'   \item{tag_id_hex}{The hexadecimal form for the tags code. This is not
#'    unique in that two fish can have the same Hex Tag Code. This is usually
#'    from the same tag code being used in different years. This is why all
#'    queries must be based on Fish ID.}
#'   \item{tag_id_decimal}{The decimal form for the tags code. This is not
#'    unique in that two fish can have the same Decimal Tag Code. This is
#'    usually from the same tag code being used in different years. This is why
#'    all queries must be based on Fish ID.}
#'   \item{tag_weight}{weight of tag in air}
#'   \item{tag_model}{The model of the tag.}
#'   \item{tag_pulse_rate_interval_nominal}{The nominal (aka approximate) pulse
#'    rate interval. This is how often the tag transmits its code signal.}
#'   \item{tag_warranty_life}{The minimum number of days a tag is expected to
#'    transmit its code. Generally tags transmit for at least 1.5x the warranty
#'    life.}
#'   \item{fish_length_type}{The way the fish was measured. Fork length = FL,
#'    total length = TL, standard length = SL. If unknown = NA.}
#'   \item{fish_length}{Measured fish length in millimeters}
#'   \item{fish_weight}{Measured fish weight in grams (in air)}
#'   \item{release_location}{The name of the place that the fish was released.}
#'   \item{release_latitude}{The latitude of the release location.}
#'   \item{release_longitude}{The longitude of the release location.}
#'   \item{release_river_km}{The river km of the release location.
#'    The Golden Gate Bridge = rkm 0.0. Values increase the further upstream.}
#'   \item{email}{the email address of the point of contact that grants approval
#'    for using the data from each fish.}
#' }
"cft_fish"

#' Example receiver data from CalFishTrack
#'
#' A dataframe of acoustic receiver metadata downloaded from ERDDAP representing
#' receivers deployed from 2021-2022.
#'
#' @format ## `cft_rcvrs`
#' A dataframe of 1,130 rows and 12 columns:
#' \describe{
#'   \item{dep_id}{a unique number that identifies a single deployment}
#'   \item{receiver_serial_number}{the serial number of the acoustic receiver
#'    that recorded the detection. If this value is 1, then the record is not a
#'    receiver detection but is the release date-time and location. Every
#'    tagged fish has at least one record in the table}
#'   \item{receiver_general_location}{A name for the geographic location of one
#'    or more receivers.}
#'   \item{receiver_region}{A name for a larger geographic region that defines a
#'    subarea of the Central Valley watershed. Some regions are Upper Sac R,
#'    Lower Sac R, Feather R, East Delta, West Delta, SF Bay.}
#'   \item{receiver_location}{The name for the location of a single receiver}
#'   \item{latitude}{Latitude for the exact location of a single receiver.
#'    If unknown = cell is blank}
#'   \item{longitude}{Longitude for the exact location of a single receiver. If
#'   unknown = cell is blank}
#'   \item{receiver_river_km}{River kilometer for the location of a single
#'    receiver. Must be unique for each GPSname. The Golden Gate Bridge is rkm 0.
#'    Values increase the further upstream}
#'   \item{receiver_make}{Describes the manufacturer and type of JSATS receiver.}
#'   \item{receiver_depth}{the estimated depth of the receiver hydrophone. If
#'    unknown = blank}
#'   \item{receiver_start}{	The date and time in Pacific Standard Time when the
#'    receiver was deployed (put at the site in the water). Format: date time
#'    M/DD/YYYY HH:MM}
#'   \item{receiver_end}{The date and time in Pacific Standard Time when the
#'    receiver was recovered (removed from the water). Format: date time
#'    M/DD/YYYY HH:MM}
#'   \item{receiver_last_valid}{	Taken from the detection data file for this
#'    deployment, the date and time in Pacific Standard Time of the last valid
#'    detection or sensor data entry. This time cannot be greater than the
#'    EndTime, but can be less that the EndTime if receiver quit recording data
#'    or was pulled out of the water. Format: date time M/DD/YYYY HH:MM}
#'   \item{receiver_data_coverage}{	describes how extensive the data is for this
#'    receiver deployment. If the receiver failed to work or was lost and not
#'    recovered = none. If it worked for part of the deployment = partial. If
#'    the receiver worked the entire deployment = full. Can be blank.}
#'   \item{receiver_coverage_problem}{If the receiver did not work until EndTime
#'    then an entry here describes the reason why the receiver may have failed.}
#'   \item{receiver_agency}{The agency (abbreviated) managing the receiver
#'    deployment.}
#'   \item{receiver_beacon_id_hex}{The hex code of the beacon tag associated
#'    with this receiver deployment}
#'   \item{	receiver_beacon_id_dec}{The decimal code of the beacon tag
#'    associated with this receiver deployment}
#'   \item{receiver_beacon_pri}{The pulse rate interval of the beacon tag in
#'    secs, usually 60 or 30 secs}
#'  }
"cft_rcvrs"

