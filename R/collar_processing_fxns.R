## PROCESS COLLAR KEYS

# Functions to pull collar data off Vectronix website, using
# the supplied collar keys, then process and clean data.

# AUTHORSHIP WHERE NOTED: Angus Smith @angus-smith


## collar data cleaning

load_collar_data <- function(collar_keys){
  
  collar_data <- collar::fetch_vectronics(collar_keys) |>
    dplyr::rename(collar_id = "idcollar",
           dttm = "acquisitiontime",
           lat = "latitude",
           long = "longitude",
           elev_m = "height",
           fix_type = "idfixtype",
           mort_status = "idmortalitystatus",
           temp_C = "temperature") |>
    #dplyr::select(collar_id, dttm, lat, long, elev_m, fix_type, mort_status, temp_C, dop) |>
    dplyr::mutate(dttm_utc = lubridate::ymd_hms(dttm), # UTC time as provided by Inventa
                  dttm_pt = lubridate::ymd_hms(dttm) |> lubridate::with_tz("Canada/Pacific"), # Local time including daylight savings
                  dttm = lubridate::ymd_hms(dttm) |> lubridate::with_tz("GMT+8"), # UTC-8 (ignores daylight savings)
                  year = lubridate::year(dttm),
                  month = lubridate::month(dttm), 
                  doy = lubridate::yday(dttm)) |>
    dplyr::select(idposition, collar_id, dttm_utc, dttm_pt, dttm, year, month, doy, dplyr::everything()) |>
    dplyr::group_by(collar_id) |>
    dplyr::arrange(dttm, .by_group = TRUE) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(lat) | !is.na(long))
  
  return(collar_data)
}

load_capture_data <- function(path){
  capture_data <- readxl::read_xlsx(path, sheet = 1, 
                                    na = c("na", "NA", "N/A", "#N/A",
                                           "Unk", "Unk*", "NR"))
  capture_data <- janitor::clean_names(capture_data)
  
  names(capture_data)[grep("monitoring_end_date", names(capture_data))] <- "monitoring_end"
  
  # Health checks
  stopifnot("Some capture dates are not provided and/or failed to parse" = all(!is.na(capture_data$capture_date)))
  stopifnot("Not all capture dates match concatenated values of capture_year-capture_month-capture_day" = all(lubridate::make_date(capture_data$capture_year, capture_data$capture_month, capture_data$capture_day) == capture_data$capture_date))
  stopifnot("Not all monitoring end dates match concatenated values of end_monitoring_year-end_monitoring_month-end_monitoring_day" = all(lubridate::make_date(capture_data$end_monitoring_year, capture_data$end_monitoring_month, capture_data$end_monitoring_day) == capture_data$monitoring_end, na.rm = TRUE))
  
  # Clean up dates
  
  # dart 1
  lubridate::tz(capture_data$dart_1) <- "America/Vancouver"
  lubridate::date(capture_data$dart_1) <- capture_data$capture_date
  
  # dart 2
  lubridate::tz(capture_data$dart_2) <- "America/Vancouver"
  lubridate::date(capture_data$dart_2) <- capture_data$capture_date
  
  # dart 3
  lubridate::tz(capture_data$dart_3) <- "America/Vancouver"
  lubridate::date(capture_data$dart_3) <- capture_data$capture_date
  
  # elk down
  lubridate::tz(capture_data$elk_down) <- "America/Vancouver"
  lubridate::date(capture_data$elk_down) <- capture_data$capture_date
  
  # reversal time
  lubridate::tz(capture_data$reversal_time) <- "America/Vancouver"
  lubridate::date(capture_data$reversal_time) <- capture_data$capture_date
  
  # standing
  lubridate::tz(capture_data$standing) <- "America/Vancouver"
  lubridate::date(capture_data$standing) <- capture_data$capture_date
  
  # Capture date
  lubridate::tz(capture_data$capture_date) <- "America/Vancouver"
  
  # monitoring end date
  lubridate::tz(capture_data$monitoring_end) <- "America/Vancouver"
  
  # Create monitoring start col
  # If elk_down is NULL, use capture_date w/o timestamp
  capture_data$monitoring_start <- dplyr::if_else(is.na(capture_data$standing), 
                                                  capture_data$elk_down, # use 'elk_down' if 'standing' is NA
                                                  capture_data$standing)
  
  # Flag any elk that were capture mortalities. They shouldn't get
  # any detections. (For now that's just 21-1887)
  capture_data$no_dets <- FALSE
  capture_data[["no_dets"]][(lubridate::date(capture_data$monitoring_start) == lubridate::date(capture_data$monitoring_end)) & grepl("MORT", capture_data$status)] <- TRUE
  
  # Ensure monitoring end is at midnight, to include any detections
  # from the day-of. So, add 1 day. 
  # E.g., monitoring end date of Oct 5 -> Oct 5 at 23:59:59/Oct 6 at 00:00:00.
  capture_data$monitoring_end <- capture_data$monitoring_end + lubridate::days(1)
  
  # Reorder cols
  capture_data <- dplyr::select(capture_data, capture_id, primary_wlh_id, 
                                serial_number, monitoring_start, monitoring_end, 
                                dplyr::everything()) |>
    dplyr::arrange(primary_wlh_id, elk_down)
  
  names(capture_data)[2] <- "animal_id"
  names(capture_data)[3] <- "collar_id"
  
  return(capture_data)
}

attribute_animal_id <- function(raw_collar_data, capture_data) {
  raw <- raw_collar_data
  out <- sqldf::sqldf("SELECT idposition, animal_id, r.collar_id, dttm, scts, origincode, ecefx, ecefy, ecefz, lat, long, elev_m, dop, fix_type, positionerror, satcount, ch01satid, ch01satcnr, ch02satid, ch02satcnr, ch03satid, ch03satcnr, ch04satid, ch04satcnr, ch05satid, ch05satcnr, ch06satid, ch06satcnr, ch07satid, ch07satcnr, ch08satid, ch08satcnr, ch09satid, ch09satcnr, ch10satid, ch10satcnr, ch11satid, ch11satcnr, ch12satid, ch12satcnr, mort_status, activity, mainvoltage, backupvoltage, temp_C, transformedx, transformedy, year, month, doy
              from raw r
              left join capture_data c
              on r.collar_id = c.collar_id 
              AND ((r.dttm >= c.monitoring_start and r.dttm <= c.monitoring_end)
                OR (r.dttm >= c.monitoring_start and c.monitoring_end is NULL))
              AND c.no_dets = FALSE;")
  
  return(out)
}

# OUTDATED
# Author: Angus Smith
# attribute_animal_id <- function(collar_data, capture_data){
#   
#   capture_data <- capture_data |>
#     dplyr::filter(collar_id %in% collar_data$collar_id)
#   
#   if(!all(collar_data$collar_id %in% capture_data$collar_id)){
#     warning("Some collar ids in collar data do not have capture data. Review supplied collar keys and capture data. \n",
#             paste("Missing collar id:", collar_data$collar_id[!collar_data$collar_id %in% capture_data$collar_id] |> unique(), "\n"))
#   }
#   
#   
#   # For loop through capture data, row by row.
#   # Identify:
#   # ○ collar id
#   # ○ start date
#   # ○ end date
#   # § If no end date, assign as latest date in the collar data (identified above)
#   # ○ animal id
#   #
#   # Select from the collar data where:
#   #   ○ the collar id is the capture row's collar id AND data within the start date and end date (inclusive)
#   # Then, for that collar data, assign the animal id to the animal id field
#   
#   collar_data$animal_id <- NA
#   
#   for (i in 1:nrow(capture_data)){
#     collar_id_temp <- capture_data$collar_id[i]
#     animal_id_temp <- capture_data$animal_id[i]
#     start_date <- capture_data$capture_date[i]
#     end_date <- capture_data$monitoring_end[i]
#     if (is.na(end_date)){
#       end_date <- max(collar_data[["dttm"]][collar_data$collar_id == collar_id_temp], na.rm = TRUE)
#     }
#     
#     collar_data[["animal_id"]][collar_data$collar_id == collar_id_temp &
#                                  collar_data$dttm >= start_date &
#                                  collar_data$dttm <= end_date] <- animal_id_temp
#   }
#   
#   # make animal id a factor
#   collar_data$animal_id <- as.factor(collar_data$animal_id)
#   
#   # remove collar data from before and after period collar was on animal
#   # SP: keep that in for data QC down the line.
#   # collar_data <- collar_data |>
#   #   dplyr::filter(!is.na(animal_id))
#   
#   # order data by animal id, dttm
#   collar_data <- collar_data |>
#     dplyr::arrange(animal_id, dttm)
#   
#   # reorder cols
#   collar_data <- collar_data |>
#     dplyr::select(animal_id, collar_id, dplyr::everything())
#   
#   return(collar_data)
# }

remove_imprecise_locations <- function(collar_data){
  # Author: Angus Smith
  # 3 = 2D, 4 = 3D, 5 = 3D Validated
  
  # Per Vectronic FAQ:
  # "A fix is considered validated if the DOP is better than 10.0 
  # and at least 5 satellites were used to calculate the fix. 
  # You can consider this fix to be of good quality."
  
  collar_data <- collar_data[((collar_data$fix_type == 4 | collar_data$fix_type == 5) & collar_data$dop < 10) 
                             | (collar_data$fix_type == 3 & collar_data$dop < 5), ]
  
  return(collar_data)
}


rarify_pts <- function(collar_data) {
  cd <- collar_data
  
  median_pts <- cd |>
    sf::st_drop_geometry() |>
    dplyr::group_by(animal_id, lubridate::date(dttm)) |>
    dplyr::summarise(n_dets = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(animal_id) |>
    dplyr::summarise(med_daily_dets = median(n_dets), .groups = "drop")
  elk_to_rarefy <- median_pts[["animal_id"]][median_pts$med_daily_dets > 8]
  # Split cd into points to rarefy and points to leave alone
  rarefy_dat <- cd[cd$animal_id %in% elk_to_rarefy, ]
  cd <- cd[!(cd$animal_id %in% elk_to_rarefy), ]
  # Rarefy the dense points
  # For elk with 8 GPS points per day, the points are collected at
  # 00:00, 03:00, 06:00, 09:00, 12:00, 15:00, 18:00, 21:00.
  keep <-
    rarefy_dat |>
    dplyr::mutate(row_id = dplyr::row_number(),
                  hour = lubridate::hour(dttm)) |> # assign hour of day to each point
      sf::st_drop_geometry() |> # drop geometry, otherwise summarise tries to merge geoms into multipoints
    dplyr::filter(hour %in% c(0, 3, 6, 9, 12, 15, 18, 21)) |> # filter to only the hours we care about
    dplyr::group_by(animal_id, doy, hour) |> # group by animal ID and day of year
    dplyr::summarise(dttm = min(dttm), # for each animal id and DOY, select the minimum timepoint
                     row_id = min(row_id),# and the associated row_id
                     .groups = "drop") |>
    dplyr::pull(row_id)
  rarefy_dat <- rarefy_dat[keep, ]
  # Merge rarefied data back to cd
  cd <- dplyr::bind_rows(cd, rarefy_dat)
  # Fix any sf issues
  cd <- suppressWarnings(sf::st_collection_extract(cd, "POINT"))
  return(cd)
}

clean_collar_data <- function(collar_data, rarify_pts = FALSE) {
  # Health check
  stopifnot("You have some detections that are not assigned to an animal_id." = all(!is.na(collar_data$animal_id)))
  
  # Clean up completely NULL columns
  collar_data <- janitor::remove_empty(collar_data, which = c("rows", "cols"))
  
  collar_data <- remove_imprecise_locations(collar_data) # 519,437 | this removes 244 bad detections
  
  # Arrange by animal_id and dttm
  collar_data <- collar_data |> dplyr::arrange(animal_id, dttm)
  
  # Make into sf object
  collar_data <- sf::st_as_sf(collar_data, coords = c("long", "lat"), crs = 4326, remove = F)
  collar_data <- sf::st_transform(collar_data, 3005)
  
  collar_data <- cbind(collar_data, sf::st_coordinates(collar_data)) # add 3005 coords as c("X", "Y") cols to the data
  
  # Next, use bayesmove::prep_data to get turning angle etc
  # NOTE EXTREMELY IMPORTANT TO ORDER YOUR DATA BY TIMESTAMP.
  # It doesn't seem that `prep_data` does this for you!!
  collar_data <- collar_data |>
    dplyr::rename(date = dttm) |>
    bayesmove::prep_data(coord.names = c("X", "Y"),
                         id = "animal_id") |>
    dplyr::rename(dttm = date) |> # rename date col back to original
    sf::st_as_sf(coords = c("x", "y"), crs = 3005) # turn back into sf
  
  # Calculate speed in m/sec and km/hr
  collar_data <- collar_data |>
    dplyr::mutate(mps = step / dt,
                  kph = mps * 3.6)
  
  # ISOLATION FOREST ROUND 1
  
  # First round will check for outlier spikes and speeds in the data.
  # Deleting these might result in new spikes to form (e.g., if two+
  # GPS points in a row were wildly off), so this will need to be
  # re-run again. In theory there should be a way to automate
  # this using a `while` loop, but this works for now.
  
  # Now let's filter out the large spikes
  
  # Flag outlier speeds
  # We're doing it per animal_id - so an animal that
  # otherwise moves very slowly over the lifetime of 
  # its track, and then has a spike of 2kph, for just
  # one single point, which technically might be feasible,
  # will still get flagged.
  isodata <- sf::st_drop_geometry(collar_data[,c("animal_id", "kph")])
  if_model <- isotree::isolation.forest(isodata, ndim = 2) # ndim = 2 to look at outliers per individual animal_id
  scores <- isotree::predict.isolation_forest(if_model, isodata, type = "avg_depth")
  
  # Merge outlier score with data
  collar_data$score_kph <- scores
  
  # After inspecting each map for each individual,
  # looks like we can comfortably flag the 5% outliers.
  collar_data$kph_pass <- collar_data$score_kph > quantile(scores, 0.05)
  
  # Looking at the dataset on the map, the largest natural/organic 
  # looking step length appears to be ~4.5 km over 3 hours, or 
  # approx 1.5km/hour.
  # Let's also fail anything that moves >2km hour.
  collar_data$kph_pass <- ifelse(collar_data$kph > 2,
                                 FALSE,
                                 collar_data$kph_pass)
  
  # Next, flag two outlier speeds in a row - this would happen in a
  # spiked movement, for example. 
  collar_data <- collar_data |>
    dplyr::mutate(kph_spike = ((dplyr::lag(kph_pass) == FALSE) & (kph_pass == FALSE)))
  
  # Finally, flag points where the turning angle is between 145-180o
  # rad * 180/pi = deg
  collar_data$sudden_angle <- abs(collar_data$angle) > 145 * (pi/180)
  
  # REMOVE OUTLIERS!
  # The `which` is important here so you don't remove NULL 'kph_spike'
  # or 'sudden_angle' - since otherwise you'd be deleting the very
  # start of each track.
  collar_data <- collar_data[which(!(collar_data$kph_spike == T & collar_data$sudden_angle == T)), ]
  
  # ISOLATION FOREST ROUND 2
  # NOW REITERATE! Now that the global outliers have been removed, 
  # recalculate the turning angles, step lengths, and speeds
  # and see if anything else gets flagged or any bad speeds remain.
  
  # Arrange by animal_id and dttm
  collar_data <- collar_data |> dplyr::arrange(animal_id, dttm)
  
  # Drop step:sudden_angle
  collar_data <- dplyr::select(collar_data, idposition:doy)
  
  # Next, use bayesmove::prep_data to get turning angle etc
  # NOTE EXTREMELY IMPORTANT TO ORDER YOUR DATA BY TIMESTAMP.
  # It doesn't seem that `prep_data` does this for you!!
  collar_data <- collar_data |>
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
    sf::st_transform(3005)
  collar_data <- cbind(collar_data, sf::st_coordinates(collar_data))
  
  collar_data <- collar_data |>
    dplyr::rename(date = dttm) |>
    bayesmove::prep_data(coord.names = c("X", "Y"),
                         id = "animal_id") |>
    dplyr::rename(dttm = date) |> # rename date col back to original
    sf::st_as_sf(coords = c("x", "y"), crs = 3005) # turn back into sf
  
  # Re-calculate speed in m/sec and km/hr
  collar_data <- collar_data |>
    dplyr::mutate(mps = step / dt,
                  kph = mps * 3.6)
  
  # Re-flag outlier speeds
  isodata <- sf::st_drop_geometry(collar_data[,c("animal_id", "kph")])
  if_model <- isotree::isolation.forest(isodata, ndim = 2) # ndim = 2 to look at outliers per individual animal_id
  scores <- isotree::predict.isolation_forest(if_model, isodata, type = "avg_depth")
  
  # Merge outlier score with data
  collar_data$score_kph <- scores
  
  # This time conservatively only flag the 1% outliers
  # (for this specific elk data, it should just be one point
  # in the entire dataset)
  collar_data$kph_pass <- collar_data$score_kph > quantile(scores, 0.01)
  
  # Looking at the dataset on the map, the largest natural/organic 
  # looking step length appears to be ~4.5 km over 3 hours, or 
  # approx 1.5km/hour.
  # Let's also fail anything that moves >2km hour.
  collar_data$kph_pass <- ifelse(collar_data$kph > 2,
                                 FALSE,
                                 collar_data$kph_pass)
  
  # Next, flag two outlier speeds in a row - this would happen in a
  # spiked movement, for example. 
  collar_data <- collar_data |>
    dplyr::mutate(kph_spike = ((dplyr::lag(kph_pass) == FALSE) & (kph_pass == FALSE)))
  
  # Finally, flag points where the turning angle is between 145-180o
  # rad * 180/pi = deg
  collar_data$sudden_angle <- abs(collar_data$angle) > 145 * (pi/180)
  
  # REMOVE OUTLIERS!
  collar_data <- collar_data[which(!(collar_data$kph_spike == T & collar_data$sudden_angle == T)), ]
  
  
  # Now, rarify the points, if that was set to TRUE
  # the fxn `rarify_pts` is defined separately above
  if (rarify_pts) {
    collar_data <- rarify_pts(collar_data)
  }
  
  # Finally, recalculate movement metrics one more time.
  # Arrange by animal_id and dttm
  collar_data <- collar_data |> dplyr::arrange(animal_id, dttm)
  
  # Drop step:sudden_angle
  collar_data <- dplyr::select(collar_data, idposition:doy)
  
  # Next, use bayesmove::prep_data to get turning angle etc
  # NOTE EXTREMELY IMPORTANT TO ORDER YOUR DATA BY TIMESTAMP.
  # It doesn't seem that `prep_data` does this for you!!
  collar_data <- collar_data |>
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
    sf::st_transform(3005)
  collar_data <- cbind(collar_data, sf::st_coordinates(collar_data))
  
  collar_data <- collar_data |>
    dplyr::rename(date = dttm) |>
    bayesmove::prep_data(coord.names = c("X", "Y"),
                         id = "animal_id") |>
    dplyr::rename(dttm = date) |> # rename date col back to original
    sf::st_as_sf(coords = c("x", "y"), crs = 3005) # turn back into sf
  
  # Calculate speed in m/sec and km/hr
  collar_data <- collar_data |>
    dplyr::mutate(mps = step / dt,
                  kph = mps * 3.6)
  
  return(collar_data)
}



# OUTDATED

# add_mvt_metrics <- function(mvt_data, 
#                             X = "long", Y = "lat",
#                             remove_names = FALSE){
#   
#   # Assign data to `X` and `Y` cols from the supplied column names
#   # to then run the next fxns
#   mvt_data$X <- mvt_data[[X]]
#   mvt_data$Y <- mvt_data[[Y]]
#   
#   ## Angus Smith 2021
#   
#   # relative and absolute direction in angles between -pi and pi
#   # step length in meters
#   # net squared displacement in meters (squared)
#   # speed in kilometer per hour
#   # difference in time in hours
#   
#   ## remove_names is to be used (TRUE) if running this on data that previously
#   ## had the names below generated. Basically, if this is a redo.
#   ## This is done when filtering the data then regenerating stats.
#   
#   if (remove_names){
#     mvt_data <- mvt_data |>
#       dplyr::select(-dir_rel, -dir_abs, -step_length_m, -NSD, -spd_kph, -DT)
#   }
#   
#   mvt_data <- mvt_data |>
#     dplyr::mutate(Year = lubridate::year(dttm))
#   
#   mvt_data <- mvt_data |>
#     dplyr::group_by(animal_id, Year = lubridate::year(dttm)) |>
#     tidyr::nest() |>
#     dplyr::mutate(dir_rel = purrr::map(data, function(x) x |>
#                            amt::make_track(.x = X,
#                                            .y = Y,
#                                            .t = dttm,
#                                            crs = 3005) |>
#                            amt::direction_rel()),
#            dir_abs = purrr::map(data, function(x) x |>
#                            amt::make_track(.x = X,
#                                            .y = Y,
#                                            .t = dttm,
#                                            crs = 3005) |>
#                            amt::direction_abs(zero_dir = "N",
#                                               clockwise = TRUE)),
#            step_length_m = purrr::map(data, function(x) x |>
#                                  amt::make_track(.x = X,
#                                                  .y = Y,
#                                                  .t = dttm,
#                                                  crs = 3005) |>
#                                  amt::step_lengths()),
#            NSD = purrr::map(data, function(x) x |>
#                        amt::make_track(.x = X,
#                                        .y = Y,
#                                        .t = dttm,
#                                        crs = 3005) |>
#                        amt::nsd()),
#            spd_kph = purrr::map(data, function(x) x |>
#                            amt::make_track(.x = X,
#                                            .y = Y,
#                                            .t = dttm,
#                                            crs = 3005) |>
#                            amt::speed() |>
#                            magrittr::multiply_by(3.6))) |>
#     tidyr::unnest(cols = c(data, dir_rel, dir_abs, step_length_m, NSD, spd_kph)) |>
#     dplyr::mutate(DT = difftime(dttm, dplyr::lag(dttm), units = "hour") |>
#              as.numeric()) |>
#     dplyr::ungroup()
#   
#   mvt_data <- mvt_data |>
#     dplyr::select(-Year, -X, -Y)
#   
#   return(mvt_data)
# }
# 
# clean_collar_data_moorter_moe <- function(collar_data, 
#                                           X = "long", Y = "lat",
#                                           window_size = 21,
#                                           mediancrit = 4000, meancrit = 6000,
#                                           spikesp = 1, spikecos = (-0.97)){
#   
#   # Assign data to `X` and `Y` cols from the supplied column names
#   # to then run the next fxns
#   collar_data$X <- collar_data[[X]]
#   collar_data$Y <- collar_data[[Y]]
#   
#   ## Angus Smith 2021
#   
#   ### TESTING ###
#   # window_size  <- 21
#   # mediancrit <- 4000
#   # meancrit <- 6000
#   # spikesp <- 1
#   # spikecos <- (-0.97)
#   
#   ## function based on work from Moorter, Van, and Christer Moe. “Screening Global Positioning System Location Data for Errors Using Animal Movement Characteristics,” n.d., 6.
#   
#   ## This function can clean up points that are:
#   ## - too far away from the rolling mean point
#   ## - too far away from the rolling median point
#   ## - likely to be spikes
#   
#   ## IF I want to look at the error points, I need to change this function. The
#   ## error points are automatically removed.
#   
#   ## I made this to work with my own data. The form of the data is presumed to
#   ## be the same as created in my collar data cleaning script.
#   
#   ## To dos for the function:
#   ## - I need to identify reasonable thresholds for the function parameters. I can
#   ## do that by reviewing the trajectories of elk movement. There are some obvious
#   ## spikes that I can use in the movements of some elk. Not percentile
#   ## threshold, as that presumes that there are always bad points.
#   ## - No such thing as a "median" centre. I couldn't find anything on this
#   ## concept. It works well enough, but be aware it seems to be a niche hacky
#   ## thing in ecology.
#   
#   
#   ## create the flags for distance of each point to the mean/median point created
#   ## from a window (default 21, centered on the point). If the point is too near
#   ## the beginning or end of the data, use the left or right versions instead.
#   ##
#   ## I think the fill = NA are useless - maybe remove?
#   collar_data <- collar_data |>
#     dplyr::group_by(animal_id) |>
#     dplyr::mutate(x_mean_left = zoo::rollapply(X,
#                                         window_size,
#                                         mean,
#                                         partial = FALSE,
#                                         fill = NA,
#                                         align = "left"),
#            x_mean_center = zoo::rollapply(X,
#                                           window_size,
#                                           mean,
#                                           partial = FALSE,
#                                           fill = NA,
#                                           align = "center"),
#            x_mean_right = zoo::rollapply(X,
#                                          window_size,
#                                          mean,
#                                          partial = FALSE,
#                                          fill = NA,
#                                          align = "right"),
#            x_mean = ifelse(!is.na(x_mean_center),
#                            yes = x_mean_center,
#                            no = ifelse(!is.na(x_mean_left),
#                                        yes = x_mean_left,
#                                        no = x_mean_right)),
#            y_mean_left = zoo::rollapply(Y,
#                                         window_size,
#                                         mean,
#                                         partial = FALSE,
#                                         fill = NA,
#                                         align = "left"),
#            y_mean_center = zoo::rollapply(Y,
#                                           window_size,
#                                           mean,
#                                           partial = FALSE,
#                                           fill = NA,
#                                           align = "center"),
#            y_mean_right = zoo::rollapply(Y,
#                                          window_size,
#                                          mean,
#                                          partial = FALSE,
#                                          fill = NA,
#                                          align = "right"),
#            y_mean = ifelse(!is.na(y_mean_center),
#                            yes = y_mean_center,
#                            no = ifelse(!is.na(y_mean_left),
#                                        yes = y_mean_left,
#                                        no = y_mean_right)),
#            ## mean flag
#            dist_to_mean = sqrt((X - x_mean)^2 + (Y - y_mean)^2),
#            mean_error = dist_to_mean > meancrit,
#            
#            x_median_left = zoo::rollapply(X,
#                                           window_size,
#                                           median,
#                                           partial = FALSE,
#                                           fill = NA,
#                                           align = "left"),
#            x_median_center = zoo::rollapply(X,
#                                             window_size,
#                                             median,
#                                             partial = FALSE,
#                                             fill = NA,
#                                             align = "center"),
#            x_median_right = zoo::rollapply(X,
#                                            window_size,
#                                            median,
#                                            partial = FALSE,
#                                            fill = NA,
#                                            align = "right"),
#            x_median = ifelse(!is.na(x_median_center),
#                              yes = x_median_center,
#                              no = ifelse(!is.na(x_median_left),
#                                          yes = x_median_left,
#                                          no = x_median_right)),
#            y_median_left = zoo::rollapply(Y,
#                                           window_size,
#                                           median,
#                                           partial = FALSE,
#                                           fill = NA,
#                                           align = "left"),
#            y_median_center = zoo::rollapply(Y,
#                                             window_size,
#                                             median,
#                                             partial = FALSE,
#                                             fill = NA,
#                                             align = "center"),
#            y_median_right = zoo::rollapply(Y,
#                                            window_size,
#                                            median,
#                                            partial = FALSE,
#                                            fill = NA,
#                                            align = "right"),
#            y_median = ifelse(!is.na(y_median_center),
#                              yes = y_median_center,
#                              no = ifelse(!is.na(y_median_left),
#                                          yes = y_median_left,
#                                          no = y_median_right)),
#            ## median flag
#            dist_to_median = sqrt((X - x_median)^2 + (Y - y_median)^2),
#            median_error = dist_to_median > mediancrit) |>
#     ## Here is where I remove the data that is flagged as bad by the median and
#     ## the mean error flags.
#     dplyr::filter(mean_error == FALSE & median_error == FALSE) |>
#     dplyr::select(1:x_mean_left) |>
#     dplyr::select(-x_mean_left) |>
#     add_mvt_metrics(remove_names = FALSE)
#   
#   ## find spikes
#   ## Make the spike error flag true or false. Only true (actual error) if the
#   ## spike meets all three criteria:
#   ## - speed of the point is greater than the threshold
#   ## - the lagged speed is greater than the threshold (same threshold as first
#   ## criteria)
#   ## - the cosine of the relative angle of the step is less than the threshold
#   ##      # NOTE: angle in radians.
#   ##      # When looking at the cosine of the relative step angle (in radians),
#   ##      # you can interpret 1 as straight (same direction), -1 as a 180 turn,
#   ##      # 0 as a 90 degree turn (left or right). It is useful for looking at
#   ##      # the directional persistence of this step vs the last one.
#   collar_data <- collar_data |>
#     dplyr::group_by(animal_id) |>
#     dplyr::mutate(spd_error = spd_kph > spikesp,
#            lag_spd_error = dplyr::lag(spd_kph) > spikesp,
#            angle_error = cos(dir_rel) < spikecos,
#            spike_error = spd_error & lag_spd_error & angle_error) |>
#     dplyr::ungroup() |>
#     dplyr::filter(spike_error == FALSE) |>
#     dplyr::select(-spd_error, -lag_spd_error, -angle_error, -spike_error) |>
#     add_mvt_metrics(remove_names = TRUE)
#   
#   return(collar_data)
# }


# OUTDATED

# # Merge all cleaning fxns into one
# 
# # rarefy_pts = should high sample rate individuals be filtered down to
# # 8 pts per day, yes or no
# clean_collar_data <- function(collar_data, rarefy_pts = TRUE) {
#   cd <- remove_imprecise_locations(collar_data)
#   cd <- clean_collar_data_moorter_moe(cd)
#   cd <- sf::st_as_sf(cd,
#                      coords = c("long", "lat"),
#                      crs = 4326,
#                      remove = FALSE) |>
#     sf::st_transform(3005)
# 
#   if (rarefy_pts) {
#     median_pts <- cd |>
#       sf::st_drop_geometry() |>
#       dplyr::group_by(animal_id, lubridate::date(dttm)) |>
#       dplyr::summarise(n_dets = dplyr::n(), .groups = "drop") |>
#       dplyr::group_by(animal_id) |>
#       dplyr::summarise(med_daily_dets = median(n_dets), .groups = "drop")
#     elk_to_rarefy <- median_pts[["animal_id"]][median_pts$med_daily_dets > 8]
#     # Split cd into points to rarefy and points to leave alone
#     rarefy_dat <- cd[cd$animal_id %in% elk_to_rarefy, ]
#     cd <- cd[!(cd$animal_id %in% elk_to_rarefy), ]
#     # Rarefy the dense points
#     # For elk with 8 GPS points per day, the points are collected at
#     # 00:00, 03:00, 06:00, 09:00, 12:00, 15:00, 18:00, 21:00.
#     keep <-
#       rarefy_dat |>
#       dplyr::mutate(row_id = dplyr::row_number(),
#                     hour = lubridate::hour(dttm)) |> # assign hour of day to each point
#         sf::st_drop_geometry() |> # drop geometry, otherwise summarise tries to merge geoms into multipoints
#       dplyr::filter(hour %in% c(0, 3, 6, 9, 12, 15, 18, 21)) |> # filter to only the hours we care about
#       dplyr::group_by(animal_id, doy, hour) |> # group by animal ID and day of year
#       dplyr::summarise(dttm = min(dttm), # for each animal id and DOY, select the minimum timepoint
#                        row_id = min(row_id),# and the associated row_id
#                        .groups = "drop") |>
#       dplyr::pull(row_id)
#     rarefy_dat <- rarefy_dat[keep, ]
#     # Merge rarefied data back to cd
#     cd <- dplyr::bind_rows(cd, rarefy_dat)
#   }
# 
#   cd <- sf::st_collection_extract(cd, "POINT")
# 
#   return(cd)
# }