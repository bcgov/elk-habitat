# Functions to calculate dynamic Brownian Bridge Movement Models
# for elk relocations

# TODO: add options to pass window and margin params
individual_dbbmm <- function(elk_dat) {
  individuals <- unique(elk_dat[["animal_id"]])
  
  hulls <- lapply(individuals, function(i) { tryCatch({
    message("Calculating dBBMM for ", i, "...")
    e <- elk_dat[which(elk_dat$animal_id == i), ]
    
    move_obj <- move2::mt_as_move2(e, 
                                   time_column = "dttm",
                                   track_id_column = "animal_id")
    move_obj <- move2::to_move(move_obj)
    
    dbbmm <- move::brownian.bridge.dyn(move_obj,
                                       raster = 100, # 100m resolution. Anything higher takes forever to run
                                       location.error = 5, # GPS error in meters
                                       ext = 10) # buffer around points in case dBBMM estimate goes "off the map"
    
    dbbmm <- move::getVolumeUD(dbbmm)
    dbbmm <- dbbmm <= 0.99 # keep it to 100% dBBMM area to be comparable with 100% MCPs
    dbbmm <- terra::rast(dbbmm) |>
      terra::as.polygons() |>
      sf::st_as_sf() |>
      sf::st_transform(3005) |>
      dplyr::filter(layer == 1) |>
      smoothr::smooth(method = "ksmooth", smoothness = 2)
    
    # Add cols of interest
    dbbmm$animal_id <- i
    dbbmm$year <- unique(elk_dat$year)
    dbbmm$area <- st_area(dbbmm)
    return(dbbmm) 
  }, # end first tryCatch {}
  error = function(i) {
    message("Error with ", i)
  }) # end tryCatch
  }) # end lapply
    
  names(hulls) <- individuals
  return(hulls)
}


elk_dbbmm <- function(elk, season, min_days) {
  # Parse seasons into POSIX dates
  years <- unique(lubridate::year(elk$dttm))
  seasons <- lapply(years, function(x) paste0(season, "-", x) |> 
                      as.POSIXct(format = "%m-%d-%Y", tz = "America/Vancouver"))
  
  # Subset to only include season of interest
  elk_seasons <- lapply(seasons, function(x) elk[which(elk$dttm >= x[1] & elk$dttm <= x[2]), ])
  
  # Drop any empty seasons (e.g., Spring 2024 would be after the default cutoff date of March 31 2024)
  elk_seasons <- Filter(function(x) dim(x) [1] > 0, elk_seasons)
  
  # Subset to only include elk_seasons with at least one fix per day
  if (!missing(min_days)) {
    if (min_days > 1) min_days <- min_days / 100 # ensure it's a percentage
    n_days_min <- floor(seasons[[1]][2] - seasons[[1]][1])
    n_days_min <- n_days_min * min_days # if we want to ensure one point per day SS, fix_days should == 1. Otherwise, if we want, e.g., 90% days covered, fix_days = 0.9
    elk_seasons <- lapply(elk_seasons, function(x) {
      tmp <- x |> 
        sf::st_drop_geometry() |> 
        dplyr::group_by(animal_id) |>
        dplyr::summarise(n_days = ceiling(max(dttm) - min(dttm))) |>
        dplyr::mutate(enough_days = n_days >= n_days_min)
      animals_to_keep <- tmp[["animal_id"]][tmp$enough_days == TRUE]
      # Now subset to only animals_to_keep
      x <- x[which(x$animal_id %in% animals_to_keep), ]
      return(x)
    })
  }
  
  # Loop through each season, then create dBBMM for each 
  # individual within that season
  tmp <- lapply(elk_seasons, individual_dbbmm)
  names(tmp) <- paste0("x", years) # R doesn't play nice with names that start w a number
  
  # Rename hulls
  # Desired output: MCP_<animal_id>_<season>_<year>
  
  # Parse season into clean name
  season <- dplyr::case_when("01-01" %in% season ~ "Winter",
                             "04-01" %in% season ~ "Spring", 
                             "06-01" %in% season ~ "Summer",
                             TRUE ~ "Unknown")
  
  # Now actually rename them
  # JK, pointless step, but keeping the code in case it's useful
  # invisible(lapply(names(tmp), function(year) {
  #   names(tmp[[year]]) <<- paste0(names(tmp[[year]]), "_", season, "_", year) |> 
  #     gsub(pattern = "_x", replacement = "_")
  #   }))
  
  # Bind into one df
  out <- lapply(tmp, dplyr::bind_rows)
  out <- out[!is.na(out)]
  filter <- lapply(out, nrow) |> unlist(use.names = FALSE) # Filter out dfs in the list with zero rows, otherwise dplyr::bind_rows fails
  filter <- filter > 0 # Filter out dfs in the list with zero rows, otherwise dplyr::bind_rows fails
  out <- out[filter] # Filter out dfs in the list with zero rows, otherwise dplyr::bind_rows fails
  out <- dplyr::bind_rows(out)
  out$season <- season
  out$elk_season <- paste0(out$animal_id, "_", out$season, "_", out$year)
  sf::st_geometry(out) <- "geometry"
  out <- out[,c("elk_season", "animal_id", "season", "year", "area", "geometry")] # Reorder cols
  
  # TODO: add option to save shp
  return(out)
}


# A few interesting plots
#ggplot(data = dBBMM, aes(x = area)) + geom_density() + facet_wrap(~ season)
#ggplot(data = dBBMM, aes(x = area)) + geom_density() + facet_grid(rows = vars(season), cols = vars(year))
#ggplot(data = dBBMM, aes(x = area, color = as.factor(year))) + geom_density() + facet_grid(rows = vars(season))
