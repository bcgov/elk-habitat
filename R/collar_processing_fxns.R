## PROCESS COLLAR KEYS

# Functions to pull collar data off Vectronix website, using
# the supplied collar keys, then process and clean data.

# AUTHORSHIP: Angus Smith @angus-smith


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
    dplyr::mutate(dttm = lubridate::ymd_hms(dttm) |> lubridate::with_tz("Canada/Pacific")) |>
    dplyr::group_by(collar_id) |>
    dplyr::arrange(dttm, .by_group = TRUE) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(lat) | !is.na(long))
  
  return(collar_data)
}

load_capture_data <- function(path){
  # SP: rewrote this fxn to include all columns
  capture_data <- readxl::read_xlsx(path, sheet = 1)
  capture_data <- janitor::clean_names(capture_data)
  
  names(capture_data)[grep("monitoring_end_date", names(capture_data))] <- "monitoring_end"
  
  # Clean up dates
  
  # dart 1
  lubridate::date(capture_data$dart_1) <- capture_data$date
  
  # dart 2
  capture_data$dart_2 <- janitor::convert_to_datetime(as.numeric(capture_data$dart_2), tz = "America/Vancouver")
  lubridate::date(capture_data$dart_2) <- capture_data$date
  
  # elk down
  capture_data$elk_down <- janitor::convert_to_datetime(as.numeric(capture_data$elk_down), tz = "America/Vancouver")
  lubridate::date(capture_data$elk_down) <- capture_data$date
  
  # reversal time
  lubridate::date(capture_data$reversal_time) <- capture_data$date
  
  # standing
  capture_data$standing <- janitor::convert_to_datetime(as.numeric(capture_data$standing), tz = "America/Vancouver")
  lubridate::date(capture_data$standing) <- capture_data$date
  
  # monitoring end date
  capture_data$monitoring_end <- janitor::convert_to_datetime(as.numeric(capture_data$monitoring_end), tz = "America/Vancouver")
  
  
  # Create capture_date col
  # If elk_down is NULL, use simply date
  capture_data$capture_date <- capture_data$elk_down
  capture_data$capture_date[is.na(capture_data$capture_date)] <- lubridate::as_datetime(capture_data$date[is.na(capture_data$elk_down)]) |> lubridate::force_tz("America/Vancouver")
  
  # Reorder cols
  capture_data <- dplyr::select(capture_data, capture_id, primary_wlh_id, 
                                serial_number, capture_date, monitoring_end, 
                                dplyr::everything()) |>
    dplyr::arrange(primary_wlh_id, elk_down)
  
  names(capture_data)[2] <- "animal_id"
  names(capture_data)[3] <- "collar_id"
  
  return(capture_data)
}

attribute_animal_id <- function(collar_data, capture_data){
  
  capture_data <- capture_data |>
    dplyr::filter(collar_id %in% collar_data$collar_id)
  
  if(!all(collar_data$collar_id %in% capture_data$collar_id)){
    warning("Some collar ids in collar data do not have capture data. Review supplied collar keys and capture data. \n",
            paste("Missing collar id:", collar_data$collar_id[!collar_data$collar_id %in% capture_data$collar_id] |> unique(), "\n"))
  }
  
  
  # For loop through capture data, row by row.
  # Identify:
  # ○ collar id
  # ○ start date
  # ○ end date
  # § If no end date, assign as latest date in the collar data (identified above)
  # ○ animal id
  #
  # Select from the collar data where:
  #   ○ the collar id is the capture row's collar id AND data within the start date and end date (inclusive)
  # Then, for that collar data, assign the animal id to the animal id field
  
  collar_data$animal_id <- NA
  
  for (i in 1:nrow(capture_data)){
    collar_id_temp <- capture_data$collar_id[i]
    animal_id_temp <- capture_data$animal_id[i]
    start_date <- capture_data$capture_date[i]
    end_date <- capture_data$monitoring_end[i]
    if (is.na(end_date)){
      end_date <- max(collar_data[["dttm"]][collar_data$collar_id == collar_id_temp], na.rm = TRUE)
    }
    
    collar_data[["animal_id"]][collar_data$collar_id == collar_id_temp &
                                 collar_data$dttm >= start_date &
                                 collar_data$dttm <= end_date] <- animal_id_temp
  }
  
  # make animal id a factor
  collar_data$animal_id <- as.factor(collar_data$animal_id)
  
  # remove collar data from before and after period collar was on animal
  # SP: keep that in for data QC down the line.
  # collar_data <- collar_data |>
  #   dplyr::filter(!is.na(animal_id))
  
  # order data by animal id, dttm
  collar_data <- collar_data |>
    dplyr::arrange(animal_id, dttm)
  
  # reorder cols
  collar_data <- collar_data |>
    dplyr::select(animal_id, collar_id, dplyr::everything())
  
  return(collar_data)
}

remove_imprecise_locations <- function(collar_data){
  # 3 = 2D, 4 = 3D, 5 = 3D Validated
  # dop/fix_type cutoffs are what I found asking around the lab.
  collar_data <- collar_data[((collar_data$fix_type == 4 | collar_data$fix_type == 5) & collar_data$dop < 10) 
                             | (collar_data$fix_type == 3 & collar_data$dop < 5), ]
  
  return(collar_data)
}

add_mvt_metrics <- function(mvt_data, 
                            X = "long", Y = "lat",
                            remove_names = FALSE){
  
  # Assign data to `X` and `Y` cols from the supplied column names
  # to then run the next fxns
  mvt_data$X <- mvt_data[[X]]
  mvt_data$Y <- mvt_data[[Y]]
  
  ## Angus Smith 2021
  
  # relative and absolute direction in angles between -pi and pi
  # step length in meters
  # net squared displacement in meters (squared)
  # speed in kilometer per hour
  # difference in time in hours
  
  ## remove_names is to be used (TRUE) if running this on data that previously
  ## had the names below generated. Basically, if this is a redo.
  ## This is done when filtering the data then regenerating stats.
  
  if (remove_names){
    mvt_data <- mvt_data |>
      dplyr::select(-dir_rel, -dir_abs, -step_length_m, -NSD, -spd_kph, -DT)
  }
  
  mvt_data <- mvt_data |>
    dplyr::mutate(Year = lubridate::year(dttm))
  
  mvt_data <- mvt_data |>
    dplyr::group_by(animal_id, Year = lubridate::year(dttm)) |>
    tidyr::nest() |>
    dplyr::mutate(dir_rel = purrr::map(data, function(x) x |>
                           amt::make_track(.x = X,
                                           .y = Y,
                                           .t = dttm,
                                           crs = 3005) |>
                           amt::direction_rel()),
           dir_abs = purrr::map(data, function(x) x |>
                           amt::make_track(.x = X,
                                           .y = Y,
                                           .t = dttm,
                                           crs = 3005) |>
                           amt::direction_abs(zero_dir = "N",
                                              clockwise = TRUE)),
           step_length_m = purrr::map(data, function(x) x |>
                                 amt::make_track(.x = X,
                                                 .y = Y,
                                                 .t = dttm,
                                                 crs = 3005) |>
                                 amt::step_lengths()),
           NSD = purrr::map(data, function(x) x |>
                       amt::make_track(.x = X,
                                       .y = Y,
                                       .t = dttm,
                                       crs = 3005) |>
                       amt::nsd()),
           spd_kph = purrr::map(data, function(x) x |>
                           amt::make_track(.x = X,
                                           .y = Y,
                                           .t = dttm,
                                           crs = 3005) |>
                           amt::speed() |>
                           magrittr::multiply_by(3.6))) |>
    tidyr::unnest(cols = c(data, dir_rel, dir_abs, step_length_m, NSD, spd_kph)) |>
    dplyr::mutate(DT = difftime(dttm, dplyr::lag(dttm), units = "hour") |>
             as.numeric()) |>
    dplyr::ungroup()
  
  mvt_data <- mvt_data |>
    dplyr::select(-Year, -X, -Y)
  
  return(mvt_data)
}

clean_collar_data_moorter_moe <- function(collar_data, 
                                          X = "long", Y = "lat",
                                          window_size = 21,
                                          mediancrit = 4000, meancrit = 6000,
                                          spikesp = 1, spikecos = (-0.97)){
  
  # Assign data to `X` and `Y` cols from the supplied column names
  # to then run the next fxns
  collar_data$X <- collar_data[[X]]
  collar_data$Y <- collar_data[[Y]]
  
  ## Angus Smith 2021
  
  ### TESTING ###
  # window_size  <- 21
  # mediancrit <- 4000
  # meancrit <- 6000
  # spikesp <- 1
  # spikecos <- (-0.97)
  
  ## function based on work from Moorter, Van, and Christer Moe. “Screening Global Positioning System Location Data for Errors Using Animal Movement Characteristics,” n.d., 6.
  
  ## This function can clean up points that are:
  ## - too far away from the rolling mean point
  ## - too far away from the rolling median point
  ## - likely to be spikes
  
  ## IF I want to look at the error points, I need to change this function. The
  ## error points are automatically removed.
  
  ## I made this to work with my own data. The form of the data is presumed to
  ## be the same as created in my collar data cleaning script.
  
  ## To dos for the function:
  ## - I need to identify reasonable thresholds for the function parameters. I can
  ## do that by reviewing the trajectories of elk movement. There are some obvious
  ## spikes that I can use in the movements of some elk. Not percentile
  ## threshold, as that presumes that there are always bad points.
  ## - No such thing as a "median" centre. I couldn't find anything on this
  ## concept. It works well enough, but be aware it seems to be a niche hacky
  ## thing in ecology.
  
  
  ## create the flags for distance of each point to the mean/median point created
  ## from a window (default 21, centered on the point). If the point is too near
  ## the beginning or end of the data, use the left or right versions instead.
  ##
  ## I think the fill = NA are useless - maybe remove?
  collar_data <- collar_data |>
    dplyr::group_by(animal_id) |>
    dplyr::mutate(x_mean_left = zoo::rollapply(X,
                                        window_size,
                                        mean,
                                        partial = FALSE,
                                        fill = NA,
                                        align = "left"),
           x_mean_center = zoo::rollapply(X,
                                          window_size,
                                          mean,
                                          partial = FALSE,
                                          fill = NA,
                                          align = "center"),
           x_mean_right = zoo::rollapply(X,
                                         window_size,
                                         mean,
                                         partial = FALSE,
                                         fill = NA,
                                         align = "right"),
           x_mean = ifelse(!is.na(x_mean_center),
                           yes = x_mean_center,
                           no = ifelse(!is.na(x_mean_left),
                                       yes = x_mean_left,
                                       no = x_mean_right)),
           y_mean_left = zoo::rollapply(Y,
                                        window_size,
                                        mean,
                                        partial = FALSE,
                                        fill = NA,
                                        align = "left"),
           y_mean_center = zoo::rollapply(Y,
                                          window_size,
                                          mean,
                                          partial = FALSE,
                                          fill = NA,
                                          align = "center"),
           y_mean_right = zoo::rollapply(Y,
                                         window_size,
                                         mean,
                                         partial = FALSE,
                                         fill = NA,
                                         align = "right"),
           y_mean = ifelse(!is.na(y_mean_center),
                           yes = y_mean_center,
                           no = ifelse(!is.na(y_mean_left),
                                       yes = y_mean_left,
                                       no = y_mean_right)),
           ## mean flag
           dist_to_mean = sqrt((X - x_mean)^2 + (Y - y_mean)^2),
           mean_error = dist_to_mean > meancrit,
           
           x_median_left = zoo::rollapply(X,
                                          window_size,
                                          median,
                                          partial = FALSE,
                                          fill = NA,
                                          align = "left"),
           x_median_center = zoo::rollapply(X,
                                            window_size,
                                            median,
                                            partial = FALSE,
                                            fill = NA,
                                            align = "center"),
           x_median_right = zoo::rollapply(X,
                                           window_size,
                                           median,
                                           partial = FALSE,
                                           fill = NA,
                                           align = "right"),
           x_median = ifelse(!is.na(x_median_center),
                             yes = x_median_center,
                             no = ifelse(!is.na(x_median_left),
                                         yes = x_median_left,
                                         no = x_median_right)),
           y_median_left = zoo::rollapply(Y,
                                          window_size,
                                          median,
                                          partial = FALSE,
                                          fill = NA,
                                          align = "left"),
           y_median_center = zoo::rollapply(Y,
                                            window_size,
                                            median,
                                            partial = FALSE,
                                            fill = NA,
                                            align = "center"),
           y_median_right = zoo::rollapply(Y,
                                           window_size,
                                           median,
                                           partial = FALSE,
                                           fill = NA,
                                           align = "right"),
           y_median = ifelse(!is.na(y_median_center),
                             yes = y_median_center,
                             no = ifelse(!is.na(y_median_left),
                                         yes = y_median_left,
                                         no = y_median_right)),
           ## median flag
           dist_to_median = sqrt((X - x_median)^2 + (Y - y_median)^2),
           median_error = dist_to_median > mediancrit) |>
    ## Here is where I remove the data that is flagged as bad by the median and
    ## the mean error flags.
    dplyr::filter(mean_error == FALSE & median_error == FALSE) |>
    dplyr::select(1:x_mean_left) |>
    dplyr::select(-x_mean_left) |>
    add_mvt_metrics(remove_names = FALSE)
  
  ## find spikes
  ## Make the spike error flag true or false. Only true (actual error) if the
  ## spike meets all three criteria:
  ## - speed of the point is greater than the threshold
  ## - the lagged speed is greater than the threshold (same threshold as first
  ## criteria)
  ## - the cosine of the relative angle of the step is less than the threshold
  ##      # NOTE: angle in radians.
  ##      # When looking at the cosine of the relative step angle (in radians),
  ##      # you can interpret 1 as straight (same direction), -1 as a 180 turn,
  ##      # 0 as a 90 degree turn (left or right). It is useful for looking at
  ##      # the directional persistence of this step vs the last one.
  collar_data <- collar_data |>
    dplyr::group_by(animal_id) |>
    dplyr::mutate(spd_error = spd_kph > spikesp,
           lag_spd_error = dplyr::lag(spd_kph) > spikesp,
           angle_error = cos(dir_rel) < spikecos,
           spike_error = spd_error & lag_spd_error & angle_error) |>
    dplyr::ungroup() |>
    dplyr::filter(spike_error == FALSE) |>
    dplyr::select(-spd_error, -lag_spd_error, -angle_error, -spike_error) |>
    add_mvt_metrics(remove_names = TRUE)
  
  return(collar_data)
}



# Merge all cleaning fxns into one

clean_collar_data <- function(collar_data) {
  
  # make sf object
  collar_data <- sf::st_as_sf(collar_data, 
                              coords = c("long", "lat"), 
                              crs = 4326,
                              remove = FALSE) |>
    sf::st_transform(3005)
  
  cd <- remove_imprecise_locations(collar_data)
  cd <- clean_collar_data_moorter_moe(cd)
  return(cd)
}