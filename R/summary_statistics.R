# Summary stats for the elk GPS dataset

# TODO: assign season to daily dataset
# For each season...
# Create a list of start/end dates for each season-year combo
# Get a sequential list of the dates between the start and end
# Spit it out
# Merge the season results
# Assign the season text to the season column
# Re-enter loop for the next season
# Spit out resulting polygon

# We need the following:
# 1. N elk online per month for the study period
# 2. Mean/median/min/max etc. N days tracked 
# 3. Fix success per elk (N detections / total expected number of detections
#     PER DEPLOYMENT - be aware to not artificially reduce detection efficiency
#     for elk that have a gap mid-deployment from being re-collared)

# 1. N elk online per month
n_elk_per_month <- function(elk) {
  n_elk_per_month <- elk |> 
    sf::st_drop_geometry() |>  # takes forever to run if you keep geoms
    dplyr::group_by(year, month) |>
    dplyr::summarise(N = dplyr::n_distinct(animal_id)) #|> dplyr::pull(N) |> summary()
  return(n_elk_per_month)
}

# 2. Mean/median/min/max detections per deployment
n_dets_per_elk <- function(elk) {
  n_dets_per_elk <- elk |>
    sf::st_drop_geometry() |>
    dplyr::group_by(animal_id, collar_id) |>
    dplyr::summarise(N_dets = dplyr::n()) |>
    dplyr::pull(N_dets) |>
    summary()
  return(n_dets_per_elk)
}

# 3. Fix success per deployment
fix_rate <- function(elk) {
  # Since not all elk have the same ping schedule, you'd need to group
  # by ping schedule and have different 100% efficiency denominator for
  # each collar id
  
  # First, calc the median daily detections per elk. Presumably,
  # unless there's a crazy malfunction, this should match the ping schedule
  # set by the researcher. Unfortunately without metadata on the schedule
  # it's otherwise impossible to know the schedule, so this is the next
  # best way to guess at that.
  
  # TODO: account for efficiency of half days, e.g. first and 
  # last days of deployment
  # TODO: account for oscillating ping schedule (e.g., 2023 elk 
  # with higher ping rate during calving season)
  efficiency <- 
    elk |>
    sf::st_drop_geometry() |>
    dplyr::group_by(animal_id, collar_id, lubridate::date(dttm)) |>
    dplyr::summarise(n_dets = dplyr::n()) |>
    dplyr::group_by(animal_id, collar_id) |>
    dplyr::summarise(actual_dets = sum(n_dets),
                     med_daily_dets = median(n_dets),
                     n_days = dplyr::n()) |>
    dplyr::mutate(n_pts_100_prct_efficiency = med_daily_dets * n_days,
                  efficiency = actual_dets / n_pts_100_prct_efficiency) #|>
    #dplyr::select(animal_id, collar_id, actual_dets, n_days, efficiency) |>
    #dplyr::rename(n_dets = actual_dets)
  
  return(efficiency)

}


# Summary functions for the resulting outputs/results, 
# plus miscellaneous helper functions

# This function summarizes the area of any seasonal polygons,
# incl. either MCP or dBBMM.
summarize_area <- function(polygons, group_by = c("year", "season"), area_unit = "ha") {
  out <- polygons |> 
    sf::st_drop_geometry() |> 
    dplyr::mutate(area = units::set_units(area, value = area_unit, mode = "standard")) |>
    dplyr::group_by_at(group_by) |> 
    dplyr::summarize(mean_area = mean(area),
                     sd = sd(area),
                     median = median(area),
                     N = dplyr::n(),
                     .groups = "keep")
  return(out)
}



# Assign season to the weekly_* polygons
# `seasons` param must be passed as a named list, where
# the names are the names of the seasons, while 
# the values are month-day pairs. 
# E.g., c(winter = c("01-01", "03-31"),
#         spring = c("04-01", "05-15"),
#         summer = c("07-01", "08-31"))
assign_weekly_seasons <- function(weekly_shp, seasons) {
  # Assign seasons to weekly data
  
  shp <- weekly_shp
  
  # Create an empty 'season' column in the data, to later
  # assign season to
  shp$season <- NA
  
  # Extract unique years from the data
  years <- unique(shp$isoyear)
  
  # This code snippet will produce
  # a little matrix for each season 
  # that's provided. Each matrix row 
  # corresponds to a year in the dataset, 
  # while the columns correspond to week
  # number. 
  seasons_years <- lapply(seasons, function(season) {
    outer(years, season, FUN = function(years, season) {
      paste0(years, "-", season)
    }) |>
      as.POSIXct(format = "%Y-%m-%d", tz = "America/Vancouver") |>
      lubridate::isoyear() |>
      matrix(length(years), length(season))
  })
  
  seasons_weeks <- lapply(seasons, function(season) {
    outer(years, season, FUN = function(years, season) {
      paste0(years, "-", season)
    }) |>
      as.POSIXct(format = "%Y-%m-%d", tz = "America/Vancouver") |>
      lubridate::isoweek() |>
      matrix(length(years), length(season))
  })
  
  year_starts <- lapply(seasons_years, function(x){ x[, 1]})
  year_ends <- lapply(seasons_years, function(x){ x[, 2]})
  
  week_starts <- lapply(seasons_weeks, function(x){ x[, 1]})
  week_ends <- lapply(seasons_weeks, function(x){ x[, 2]})
  
  # Now, using start and end years + weeks, assign the
  # season to the supplied shp dataframe.
  # lapply loop through each of the supplied seasons
  lapply(names(seasons), function(szn) {
    # Pull the list of week starts for that given season (`szn`)
    szn_week_starts <- week_starts[[szn]]
    szn_week_ends <- week_ends[[szn]]
    
    # Then loop through the week starts for that season (`szn`)
    # and combine with the week ends to get a list of year-week
    # combos for each season
    yr_wk_list <- lapply(1:length(szn_week_starts), function(w_index) {
      # Pull the starting and ending week numbers
      w_start <- szn_week_starts[w_index]
      w_end <- szn_week_ends[w_index]
      if (tolower(szn) == "winter" & w_start > 40) {
        # Because winter can cross the year line, if the week_start number
        # is greater than 40 (the first week of november), we need to build
        # the week range in a special way to include the last 1-2 weeks of the year.
        
        # Just in case assume 53 is always the last/max week of the year
        week_range_1 <- w_start:53
        week_range_2 <- 1:w_end
        # Assign the correct ISO year to the separate week ranges
        week_range_1 <- paste0(year_starts[[szn]][w_index], ".", week_range_1)
        week_range_2 <- paste0(year_ends[[szn]][w_index], ".", week_range_2)
        # Combine
        week_range <- c(week_range_1, week_range_2)
        
      } else {
        # Else, if it's not a winter that crosses the year line,
        # we can simply pull together the week range from w_start 
        # and w_end with no issue.
        week_range <- w_start:w_end
        
        # Assign the year to the week range
        # Then, because we're not crossing the year line, we can just
        # use the year in year_starts and assign it, rather than 
        # having to pull from both year_starts and year_ends.
        week_range <- paste0(year_starts[[szn]][w_index], ".", week_range)
        
      }
      
      # And finally don't forget to return week_range!
      return(week_range)
    })
    
    yr_wk_list <- unlist(yr_wk_list)
    
    # Finally, assign the season to the shp!
    shp[["season"]][shp$isoyear_week %in% yr_wk_list] <<- stringr::str_to_title(szn)
    
  })
  
  return(shp)
}


# Assign season to the daily_* polygons
# `seasons` param must be passed as a named list, where
# the names are the names of the seasons, while 
# the values are month-day pairs. 
# E.g., c(winter = c("01-01", "03-31"),
#         spring = c("04-01", "05-15"),
#         summer = c("07-01", "08-31"))
assign_daily_seasons <- function(daily_shp, seasons) {
  # Assign seasons to daily data
  shp <- daily_shp
  
  # Create an empty 'season' column in the data, to later
  # assign season to
  shp$season <- NA
  
  # Extract unique years from the data
  years <- unique(shp$year)
  
  # This code snippet will produce
  # a little matrix for each season 
  # that's provided. Each matrix row 
  # corresponds to a year in the dataset, 
  # while the columns correspond to min
  # date and max date of the season.
  seasons_years <- lapply(seasons, function(season) {
    outer(years, season, FUN = function(years, season) {
      paste0(years, "-", season)
    }) 
  })
  
  # For each start and end date within each
  # season, extract the full sequence of
  # dates between the start and end dates (inclusive).
  # Then just collapse those dates into one 
  # giant list containing every date for 
  # every year for each season.
  seasons_dates <- lapply(seasons_years, function(season) {
    n_years <- nrow(season)
    dates_x <- lapply(1:n_years, function(year_x) {
      start_x <- season[year_x, 1]
      end_x <- season[year_x, 2]
      dates_x <- seq(lubridate::ymd(start_x),
                     lubridate::ymd(end_x), 
                     by = 'days')
    })
    out_dates <- do.call(c, dates_x) # use c() fxn to merge all the dates for all years of that season into one list
    return(out_dates)
  })
  
  # Finally assign each day within the 
  # `shp` dataset a season. Assume the 
  # name of the dates list == the name
  # of the season itself.
  invisible(
    lapply(names(seasons_dates), function(szn) {
      dates_szn <- seasons_dates[[szn]]
      shp[["season"]][shp$date %in% dates_szn] <<- stringr::str_to_title(szn)
    })
  )
  
  return(shp)
}
