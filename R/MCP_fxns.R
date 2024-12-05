# Functions to calculate minimum convex polygons for elk
# relocations 

individual_mcp <- function(elk_dat) {
  individuals <- unique(elk_dat[["animal_id"]])
  hulls <- lapply(individuals, function(i) {
    e <- elk_dat[which(elk_dat$animal_id == i), ]
    e <- e |> 
      sf::st_union() |> 
      sf::st_convex_hull() |>
      sf::st_as_sf()
    # Add cols of interest
    e$animal_id <- i
    e$year <- unique(elk_dat$year)
    e$area <- sf::st_area(e)
    return(e)
  })
  names(hulls) <- individuals
  return(hulls)
}


elk_mcp <- function(elk, season, min_days) {
  # Parse seasons into POSIX dates
  years <- unique(lubridate::year(elk$dttm))
  seasons <- lapply(years, function(x) paste0(season, "-", x) |> 
                      as.POSIXct(format = "%m-%d-%Y", tz = "America/Vancouver"))
  
  # Subset to only include season of interest
  elk_seasons <- lapply(seasons, function(x) elk[which(elk$dttm >= x[1] & elk$dttm <= x[2]), ])
  
  # Subset to only include elk_seasons with fixes on at least X% of days
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
  
  # Loop through each season, then create MCP for each 
  # individual within that season
  tmp <- lapply(elk_seasons, individual_mcp)
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

summarize_area <- function(polygons) {
  out <- polygons |> 
    sf::st_drop_geometry() |> 
    dplyr::mutate(area = units::set_units(area, "km2")) |>
    dplyr::group_by(season, year) |> 
    dplyr::summarize(mean_area = mean(area),
                     sd = sd(area),
                     median = median(area),
                     N = dplyr::n())
  return(out)
}


# A few interesting plots
#ggplot(data = MCP, aes(x = area)) + geom_density() + facet_wrap(~ season)
#ggplot(data = MCP, aes(x = area)) + geom_density() + facet_grid(rows = vars(season), cols = vars(year))
#ggplot(data = MCP, aes(x = area, color = as.factor(year))) + geom_density() + facet_grid(rows = vars(season))

