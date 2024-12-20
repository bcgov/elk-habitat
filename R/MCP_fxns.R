# Functions to calculate minimum convex polygons for elk
# relocations 

# This function summarizes the area of any seasonal polygons,
# incl. either MCP or dBBMM.
summarize_area <- function(polygons, area_unit = "ha") {
  out <- polygons |> 
    sf::st_drop_geometry() |> 
    dplyr::mutate(area = units::set_units(area, value = area_unit, mode = "standard")) |>
    dplyr::group_by(season, year) |> 
    dplyr::summarize(mean_area = mean(area),
                     sd = sd(area),
                     median = median(area),
                     N = dplyr::n())
  return(out)
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
  e$year <- unique(elk_dat$year)
  e$area <- units::set_units(sf::st_area(e), value = area_unit, mode = "standard")
  
  return(e)
  
}

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
  
  # Loop through each season, then create MCP for each 
  # individual within that season
  # First unpack dots to check if percent cutoff and center method supplied
  dots <- list(...)
  args <- match(names(formals(individual_mcp)), names(dots))
  mcp_dots <- dots[args[!is.na(args)]]
  # if ("percent" %in% names(dots)) percent <- dots$percent
  # if ("method" %in% names(dots)) method <- dots$method
  # # I'm sure there's a better way of doing this, but... this is it for now
  # # Perhaps match.call?
  # # https://stackoverflow.com/questions/64156034/handling-missing-arguments-handled-with-missing-to-call-a-function-inside-a
  # if (exists("percent") & exists("method")) {
  #   tmp <- lapply(elk_seasons, individual_mcp, percent = percent, method = method)
  # } else if (exists("percent")) {
  #   tmp <- lapply(elk_seasons, individual_mcp, percent = percent)
  # } else {
  #   tmp <- lapply(elk_seasons, individual_mcp)
  # }
  
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
#ggplot(data = MCP, aes(x = area)) + geom_density() + facet_wrap(~ season)
#ggplot(data = MCP, aes(x = area)) + geom_density() + facet_grid(rows = vars(season), cols = vars(year))
#ggplot(data = MCP, aes(x = area, color = as.factor(year))) + geom_density() + facet_grid(rows = vars(season))

