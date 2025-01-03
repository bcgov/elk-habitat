# Functions to calculate dynamic Brownian Bridge Movement Models
# for elk relocations

individual_dbbmm <- function(elk_dat, margin = 11, window.size = 31, 
                             res = 100, location.error = 5, ud_percent = 0.99, area_unit = "ha") {
  
    move_obj <- move2::mt_as_move2(elk_dat, 
                                   time_column = "dttm",
                                   track_id_column = "animal_id")
    move_obj <- move2::to_move(move_obj)
    
    dbbmm <- move::brownian.bridge.dyn(move_obj,
                                       margin = margin,
                                       window.size = window.size,
                                       raster = res, # 100m resolution. Anything higher takes forever to run
                                       location.error = location.error, # GPS error in meters
                                       ext = 30) # buffer around points in case dBBMM estimate goes "off the map"
    
    dbbmm <- move::getVolumeUD(dbbmm)
    dbbmm <- dbbmm <= ud_percent # default percent is 99% (almost all area extracted)
    dbbmm <- terra::rast(dbbmm) |>
      terra::as.polygons() |>
      sf::st_as_sf() |>
      sf::st_transform(3005) |>
      dplyr::filter(layer == 1) |>
      smoothr::smooth(method = "ksmooth", smoothness = 2)
    
    # Add cols of interest
    dbbmm$animal_id <- unique(elk_dat$animal_id)
    dbbmm$year <- unique(elk_dat$year)
    dbbmm$area <- units::set_units(sf::st_area(dbbmm), value = area_unit, mode = "standard")
    return(dbbmm) 
  
}


seasonal_dbbmm <- function(elk, season, min_days = NA, ...) {
  # Dots pass arguments to individual_dbbmm - margin, window.size, res,
  # location.error, ud_percent, and area_unit
  
  # Parse seasons into POSIX dates
  years <- unique(lubridate::year(elk$dttm))
  seasons <- lapply(years, function(x) paste0(season, "-", x) |> 
                      as.POSIXct(format = "%m-%d-%Y", tz = "America/Vancouver"))
  
  # Subset to only include season of interest
  elk_seasons <- lapply(seasons, function(x) elk[which(elk$dttm >= x[1] & elk$dttm <= x[2]), ])
  names(elk_seasons) <- paste0("x", years) # R doesn't play nice with names that start w a number
  
  # Drop any empty seasons (e.g., Spring 2024 would be after the default cutoff date of March 31 2024)
  elk_seasons <- Filter(function(x) dim(x) [1] > 0, elk_seasons)
  
  # Subset to only include elk_seasons with at least one fix per day
  if (!is.na(min_days)) {
    if (min_days > 1) min_days <- min_days / 100 # ensure it's a percentage
    n_days_min <- floor(seasons[[1]][2] - seasons[[1]][1])
    n_days_min <- n_days_min * min_days # if we want to ensure one point per day SS, fix_days should == 1. Otherwise, if we want, e.g., 90% days covered, fix_days = 0.9
    elk_seasons <- lapply(elk_seasons, function(x) {
      tmp <- x |> 
        sf::st_drop_geometry() |>
        dplyr::mutate(date = lubridate::date(dttm)) |>
        dplyr::select(animal_id, date) |>
        dplyr::distinct() |>
        dplyr::group_by(animal_id) |>
        dplyr::summarise(n_days = dplyr::n()) |>
        dplyr::mutate(enough_days = n_days >= n_days_min)
      animals_to_keep <- tmp[["animal_id"]][tmp$enough_days == TRUE]
      # Now subset to only animals_to_keep
      x <- x[which(x$animal_id %in% animals_to_keep), ]
      return(x)
    })
  }
  
  # Once again drop any empty seasons (can happen if you're running
  # this function on a single individual)
  elk_seasons <- Filter(function(x) dim(x) [1] > 0, elk_seasons)
  
  # Loop through each season, then create dBBMM for each 
  # individual within that season
  
  # First unpack dots to check if percent cutoff and center method supplied
  dots <- list(...)
  args <- match(names(formals(individual_dbbmm)), names(dots))
  dbbmm_dots <- dots[args[!is.na(args)]]
  
  tmp <- lapply(elk_seasons, function(x) {
    elk_dat <- x
    individuals <- unique(elk_dat[["animal_id"]])
    
    hulls <- lapply(individuals, function(i) { tryCatch({
      message("Calculating dBBMM for ", i, "...")
      # Subset to individual
      e <- elk_dat[which(elk_dat$animal_id == i), ]
      # Calculate dBBMM
      if (length(dbbmm_dots) == 0) {
        out <- individual_dbbmm(elk_dat = e)
      } else {
        out <- do.call("individual_dbbmm", args = c(list(e), dbbmm_dots))
      }
      return(out)
      
    }, # end first tryCatch {}
    error = function(i) {
      message("Error with ", i)
    }) # end tryCatch
    }) # end lapply
    
    names(hulls) <- individuals
    return(hulls)
  })
  
  names(tmp) <- names(elk_seasons)
  
  # Rename hulls
  # Desired output: MCP_<animal_id>_<season>_<year>
  
  # Parse season into clean name
  season <- dplyr::case_when("01-01" %in% season ~ "Winter",
                             "04-01" %in% season ~ "Spring", 
                             "07-01" %in% season ~ "Summer",
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
  
  return(out)
}




# A few interesting plots
#ggplot(data = dBBMM, aes(x = area)) + geom_density() + facet_wrap(~ season)
#ggplot(data = dBBMM, aes(x = area)) + geom_density() + facet_grid(rows = vars(season), cols = vars(year))
#ggplot(data = dBBMM, aes(x = area, color = as.factor(year))) + geom_density() + facet_grid(rows = vars(season))
