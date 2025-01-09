# Functions to calculate minimum convex polygons for elk
# relocations 

# This function groups elk data into weekly bins, including accounting
# for partial weeks at the end/start of the year (i.e., merging the 
# final few days of December with the first few days of January into
# one single 7-day week.) Weekly binned elk data are then used for either
# MCPs or dBBMMs.
weekly_binning <- function(elk, min_days = 1, min_dets_per_day = 7) {
  # First split up data into weekly bins
  elk$isoyear <- lubridate::isoyear(elk$dttm) # gotta use isoyear so that the last week of December/first week of Jan is still chopped up into an even 7 days! E.g. cases where Dec 31 is on, for example, a Tuesday. So week 53 = 3 days long while week 1 = 4 days long.
  elk$week <- lubridate::isoweek(elk$dttm) # similar to isoyear, need to use isoweek for cases where last week of Dec is split in half
  elk_weekly <- split(elk, list(elk$isoyear, elk$week))
  
  # Next subset to only include elk_weekly with fixes on at least X% of days
  # For weekly, by default it's 100% of days
  if (min_days > 1) min_days <- min_days / 100 # ensure it's a percentage
  n_days_min <- ceiling(7 * min_days) # 7 days in a week
  #n_dets_min <- n_days_min * min_dets_per_day
  # Drop any empty dataframes in the weekly list
  elk_weekly <- Filter(function(x) dim(x)[1] > 0, elk_weekly)
  elk_weekly <- lapply(elk_weekly, function(x) {
    # Group by animal_id + day, and if there's any days with < min_dets_per_day 
    # detections, chuck the whole week.
    animals_to_keep <- 
      x |>
      sf::st_drop_geometry() |>
      dplyr::mutate(date = lubridate::date(dttm)) |>
      dplyr::select(animal_id, week, date) |>
      dplyr::group_by(animal_id, week, date) |>
      dplyr::summarise(n_dets_per_day = dplyr::n(), .groups = "keep") |>
      dplyr::filter(n_dets_per_day >= min_dets_per_day) |> # filter out days with less than min dets cutoff
      dplyr::group_by(animal_id, week) |> # next filter out any animal_ids with not enough total days
      dplyr::summarise(n_days = dplyr::n(), .groups = "drop") |>
      dplyr::filter(n_days >= n_days_min) |>
      dplyr::pull(animal_id)
    # Now subset to only animals_to_keep
    x <- x[which(x$animal_id %in% animals_to_keep), ]
    return(x)
  })
  # Drop any subsequently empty dataframes in the weekly list
  elk_weekly <- Filter(function(x) dim(x)[1] > 0, elk_weekly)
  return(elk_weekly)
}


find_centre <- find_center <- function(coords, method = "delaunay") {
  xy <- coords
  # 3 options: Delaunay triangulation, mean, or median geographic center.
  if (method == "delaunay") {
    
    # See: https://stackoverflow.com/questions/65890739/centroid-of-the-minimum-convex-polygon
    xy <- unique(xy) # tripack voronoi fails if there are duplicate points
    
    vm <- tripack::voronoi.mosaic(xy.coords(xy)) # Extract coordinates object for this function
    x <- with(vm, rowMeans(cbind(xy[p1, 1], xy[p2, 1], xy[p3, 1])))
    y <- with(vm, rowMeans(cbind(xy[p1, 2], xy[p2, 2], xy[p3, 2])))
    center <- apply(cbind(x, y), 2, weighted.mean, vm$area)  # centroid of conv hull
    
    center <- sf::st_point(x = center) |>
      sf::st_sfc(crs = 3005)
  } else if (method == "mean") {
    center <- sf::st_point(x = c(mean(xy[,1]), mean(xy[,2]))) |>
      sf::st_sfc(crs = 3005)
  } else if (method == "median") {
    center <- sf::st_point(x = c(median(xy[,1]), median(xy[,2]))) |>
      sf::st_sfc(crs = 3005)
  }
  
  return(center)
}

individual_mcp <- function(elk_dat, percent = 0.99, area_unit = "ha",...) {
  
  e <- elk_dat
  # Dots = args to pass on to `find_center` (i.e. `method` arg)
  # Unpack dots, see if a percent and/or alternative method was supplied
  dots <- list(...)
  if ("method" %in% names(dots)) method <- dots$method
  
  if (percent > 1) percent <- percent / 100 # ensure percent is btwn 0 and 1
  if (percent < 1) { 
    # Only bother calculating center/distance to center etc. if the user
    # specifies they want an MCP for <100% of the points
    # Calculate the geographic center of the points
    center <- if (exists("method")) find_center(sf::st_coordinates(e, method = method)) else find_center(sf::st_coordinates(e))
    # Calc distance to center
    e$distance_to_center <- sf::st_distance(e, center)
    distance_cutoff <- quantile(e$distance_to_center, percent)
    # Cut out any points >distance_cutoff
    e <- e[e$distance_to_center <= distance_cutoff, ]
  } 
  
  # Calculate convex hull
  e <- e |> 
    sf::st_union() |> 
    sf::st_convex_hull() |>
    sf::st_as_sf()
  
  # Add cols of interest
  e$animal_id <- unique(elk_dat$animal_id)
  #e$year <- unique(elk_dat$year) # this fails in cases where last week of the year contains two years. Better to let the user assign the year how they want after the fact
  e$area <- units::set_units(sf::st_area(e), value = area_unit, mode = "standard")
  
  return(e)
  
}

# min_days is expressed as a percentage. What percentage of days must have a detection
# in order to accept that subset of data for the MCP?
seasonal_mcp <- function(elk, season, min_days, ...) {
  # Dots = args to pass on to `individual_mcp` (percent, area_unit) and `find_center` (method)
  # Parse seasons into POSIX dates
  years <- unique(lubridate::year(elk$dttm))
  seasons <- lapply(years, function(x) paste0(season, "-", x) |> 
                      as.POSIXct(format = "%m-%d-%Y", tz = "America/Vancouver"))
  
  # Subset to only include season of interest
  elk_seasons <- lapply(seasons, function(x) elk[which(elk$dttm >= x[1] & elk$dttm <= x[2]), ])
  names(elk_seasons) <- paste0("x", years) # R doesn't play nice with names that start w a number
  
  # Drop any empty seasons (e.g., Spring 2024 would be after the default cutoff date of March 31 2024)
  elk_seasons <- Filter(function(x) dim(x) [1] > 0, elk_seasons)
  
  # Subset to only include elk_seasons with fixes on at least X% of days
  if (!missing(min_days)) {
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
  
  # First unpack dots to check if percent cutoff and center method supplied
  dots <- list(...)
  args <- match(names(formals(individual_mcp)), names(dots))
  mcp_dots <- dots[args[!is.na(args)]]
  
  # Loop through each season, then create MCP for each 
  # individual within that season
  tmp <- lapply(elk_seasons, function(x) {
    elk_dat <- x
    individuals <- unique(elk_dat[["animal_id"]])
    hulls <- lapply(individuals, function(i) { tryCatch({
      message("Calculating MCP for ", i, "...")
      # Subset to individual
      e <- elk_dat[which(elk_dat$animal_id == i), ]
      # Calculate MCP
      if (length(mcp_dots) == 0) {
        out <- individual_mcp(elk_dat = e)
      } else {
        out <- do.call("individual_mcp", args = c(list(e), mcp_dots))
      }
      return(out)
    }, # end first tryCatch {}
    error = function(i) {
      message("Error with ", i)
    }) # end tryCatch
    }) # end hulls lapply
    
    names(hulls) <- individuals
    return(hulls)
       
  }) # end tmp lapply
  
  # Rename hulls
  # Desired output: MCP_<animal_id>_<season>_<year>
  
  # Parse season into clean name
  season_txt <- dplyr::case_when("01-01" %in% season ~ "Winter",
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
  # Assign year to the polygons
  invisible(lapply(names(out), function(x) {
    year <- as.numeric(gsub("x", "", x))
    out[[x]]$year <<- year
  }))
  out <- dplyr::bind_rows(out)
  out$season <- season_txt
  out$elk_season <- paste0(out$animal_id, "_", out$season, "_", out$year)
  sf::st_geometry(out) <- "geometry"
  out <- out[,c("elk_season", "animal_id", "season", "year", "area", "geometry")] # Reorder cols

  return(out)
}

# min_days is expressed as a percentage. What percentage of days must have a detection
# in order to accept that subset of data for the MCP?
weekly_mcp <- function(elk,...) {
  # Dots = args to pass on to:
  # -> `weekly_binning` (min_days = 1, min_dets_per_day = 7)
  # -> `individual_mcp` (percent, area_unit) 
  # |-> `find_center` (method) - supplied via `individual_mcp`
  
  # First unpack dots to check if weekly binning options, 
  # percent cutoff and center method supplied
  dots <- list(...)
  # weekly_binning args
  wb_args <- match(names(formals(weekly_binning)), names(dots))
  wb_dots <- dots[wb_args[!is.na(wb_args)]]
  # individual_mcp args
  imcp_args <- match(names(formals(individual_mcp)), names(dots))
  imcp_dots <- dots[imcp_args[!is.na(imcp_args)]]
  
  # Now actually run the functions
  # First split up data into weekly bins
  if (length(wb_dots) == 0) {
    elk_weekly <- weekly_binning(elk = elk)
  } else {
    elk_weekly <- do.call("weekly_binning", args = c(list(elk), wb_dots))
  }
  
  # Loop through each week, then create MCP for each 
  # individual within that season
  tmp <- pbapply::pblapply(elk_weekly, function(x) {
    elk_dat <- x
    week <- unique(elk_dat$week)
    individuals <- unique(elk_dat[["animal_id"]])
    hulls <- lapply(individuals, function(i) { tryCatch({
      #message("Calculating MCP for ", i, "...")
      # Subset to individual
      e <- elk_dat[which(elk_dat$animal_id == i), ]
      # Calculate MCP
      if (length(imcp_dots) == 0) {
        out <- individual_mcp(elk_dat = e)
      } else {
        out <- do.call("individual_mcp", args = c(list(e), imcp_dots))
      }
      out$week <- week
      return(out)
    }, # end first tryCatch {}
    error = function(i) {
      message("Error with ", i)
    }) # end tryCatch
    }) # end hulls lapply
    
    names(hulls) <- individuals
    return(hulls)
    
  }) # end tmp lapply
  
  # Bind into one df
  out <- lapply(tmp, dplyr::bind_rows)
  out <- out[!is.na(out)]
  filter <- lapply(out, nrow) |> unlist(use.names = FALSE) # Filter out dfs in the list with zero rows, otherwise dplyr::bind_rows fails
  filter <- filter > 0 # Filter out dfs in the list with zero rows, otherwise dplyr::bind_rows fails
  out <- out[filter] # Filter out dfs in the list with zero rows, otherwise dplyr::bind_rows fails
  # Assign year to the polygons
  invisible(lapply(names(out), function(x) {
    out[[x]]$isoyear_week <<- x
  }))
  out <- dplyr::bind_rows(out)
  out$isoyear <- as.numeric(stringr::str_split(out$isoyear_week, "\\.", simplify = TRUE)[,1]) # extract year
  out <- out[,c("animal_id", "isoyear", "week", "isoyear_week", "area", "x")] # Reorder cols
  sf::st_geometry(out) <- "geometry" # Rename geometry column to "geometry"
  
  return(out)
  
}


daily_mcp <- function(elk, min_dets_per_day = 8, ...) {
  # Dots = args to pass on to `individual_mcp` (percent, area_unit) or `find_center` (method)
  
  # Add date column
  elk$date <- lubridate::date(elk$dttm)
  
  # Next subset to only include elk data with 8 fixes (100%) per day
  # It's fastest to filter the data after dropping the geometry. 
  # But, we can't directly filter the elk data because we need to keep
  # that geometry for the MCP operation. So, create a `tmp` df that 
  # contains the number of fixes per day, then extract a `keep_yn` vector
  # that will be used to quickly filter the full `elk` dataframe with geometry.
  tmp <- elk |> 
    sf::st_drop_geometry() |>
    dplyr::group_by(animal_id, date) |>
    dplyr::mutate(n_dets = dplyr::n()) |>
    dplyr::select(animal_id, date, n_dets)
  keep_yn <- tmp$n_dets >= min_dets_per_day # create filter vector
  
  # Filter the main elk df
  elk <- elk[keep_yn,]
  
  # Group by animal_id and date and run MCP fxn
  # First unpack dots to check if percent cutoff and center method supplied
  dots <- list(...)
  args <- match(names(formals(individual_mcp)), names(dots))
  mcp_dots <- dots[args[!is.na(args)]]
  
  # Create list of animal-date combos to iterate over
  elk$animal_date <- paste0(elk$animal_id, "_", elk$date)
  animal_dates <- unique(elk$animal_date)
  
  tmp <- pbapply::pblapply(animal_dates, function(x) { 
    tryCatch({
      elk_dat <- elk[which(elk$animal_date == x), ]
      date <- unique(elk_dat$date)
      # Calculate MCP
      if (length(mcp_dots) == 0) {
        out <- individual_mcp(elk_dat = elk_dat)
      } else {
        out <- do.call("individual_mcp", args = c(list(elk_dat), mcp_dots))
      }
      out$date <- date
      return(out)
    }, # end first tryCatch {}
    error = function(x) {
      message("Error with ", x)
    }) # end tryCatch
  }) # end tmp lapply
  
  names(tmp) <- animal_dates # for troubleshooting...
  
  # Bind into one df
  out <- dplyr::bind_rows(tmp)
  out$year <- lubridate::year(out$date)
  out <- out[,c("animal_id", "year", "date", "area", "x")] # Reorder cols
  sf::st_geometry(out) <- "geometry" # Rename geometry column to "geometry"
  
  return(out)
  
}

# A few interesting plots
#ggplot(data = MCP, aes(x = area)) + geom_density() + facet_wrap(~ season)
#ggplot(data = MCP, aes(x = area)) + geom_density() + facet_grid(rows = vars(season), cols = vars(year))
#ggplot(data = MCP, aes(x = area, color = as.factor(year))) + geom_density() + facet_grid(rows = vars(season))

