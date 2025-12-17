
calc_disturbance_lyr <- function(vri, depletions, change_detection) {
  ## Setup ##
  deps <- depletions
  cd <- change_detection
  ## Prepare VRI ##
  # Create a disturbance date col
  vri$disturbance_date <- dplyr::if_else(is.na(vri$HARVEST_DATE),
                                         vri$EARLIEST_NONLOGGING_DIST_DATE,
                                         vri$HARVEST_DATE)
  
  # For cases where there is no recorded harvest date, calculate
  # projected date minus projected age to generate a projected disturbance
  # date
  vri$proj_dist_date <- vri$PROJECTED_DATE - lubridate::years(vri$PROJ_AGE_1)
  
  # There is lots of inconsistency between A) the provided harvest date
  # and B) my calculated harvest date (2024 minus PROJ_AGE_1 years).
  
  # Since this is a disturbance layer let's just go with the provided
  # disturbance date when available. This will include non-logging disturbance. 
  
  # In cases where no HARVEST or NONLOGGING disturbance date is provided, 
  # use the one I've calculated. 
  vri$disturbance_date <- dplyr::if_else(is.na(vri$disturbance_date),
                                         vri$proj_dist_date,
                                         vri$disturbance_date)
  
  # Create col with just disturbance year
  vri$disturbance_year <- as.integer(lubridate::year(vri$disturbance_date))
  
  ## Prepare depletions ##
  # This one is simple. Just make a matching disturbance_year col.
  deps$disturbance_year <- deps$Depletion_Year
  
  ## Rasterize ##
  # First check everything is the same CRS
  same_crs <- all(sf::st_crs(vri) == sf::st_crs(deps),
                  sf::st_crs(deps) == sf::st_crs(cd))
  
  if (!same_crs) stop("Supplied VRI, depletions, and change detection have differing CRS.")
  
  # Rasterize VRI + deps
  # Drop NA disturbance year polygons
  vri <- vri[which(!is.na(vri$disturbance_year)), ]
  
  # Figure out max extent that encompasses all 3 layers
  bounds <- c(xmin = min(sf::st_bbox(vri)[1], sf::st_bbox(deps)[1], sf::st_bbox(cd)[1]),
              ymin = min(sf::st_bbox(vri)[2], sf::st_bbox(deps)[2], sf::st_bbox(cd)[2]),
              xmax = max(sf::st_bbox(vri)[3], sf::st_bbox(deps)[3], sf::st_bbox(cd)[3]),
              ymax = max(sf::st_bbox(vri)[4], sf::st_bbox(deps)[4], sf::st_bbox(cd)[4]))
  bounds <- bounds |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_as_sf(crs = sf::st_crs(vri)) |>
    raster::extent() # convert to `raster` pkg type extent object

  # Create a raster template following the resolution & CRS of `cd`
  temp <- raster::raster(bounds, # the extent will be equal to the bounds calculated in `bounds`
                         res = terra::res(cd),
                         crs = terra::crs(cd))
  
  # Now rasterize the two sf polygon layers
  # VRI
  vri_dist_year <- fasterize::fasterize(vri, temp, field = "disturbance_year")
  vri_dist_year <- terra::rast(vri_dist_year)
  
  # deps
  deps_dist_year <- fasterize::fasterize(deps, temp, field = "disturbance_year")
  deps_dist_year <- terra::rast(deps_dist_year)
  
  # Modify the extent to match the others
  cd <- terra::resample(cd, vri_dist_year)
  names(cd) <- "disturbance_year"
  
  # Merge VRI, deps, cd
  # Create our disturbance raster `d` 
  # Order of data preference: between `vri` disturbance year and `deps`
  # disturbance year, preferentially choose the `deps` year
  d <- terra::merge(deps_dist_year, vri_dist_year, first = TRUE)
  # Between the `cd` disturbance year and `deps`/`vri`, preferentially choose
  # the `cd` year.
  d <- terra::merge(cd, d, first = TRUE)
  
  # Return
  return(d)
}
