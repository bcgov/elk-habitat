# Copyright 2026 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


# Download Maltman et al. (2023) 30m resolution dataset,
# crop it to the study area, and save it to the "GIS/Forest age" dir.
# https://www.sciencedirect.com/science/article/pii/S0034425723000809
# https://opendata.nfis.org/mapserver/nfis-change_eng.html
download_forest_age <- function(url, aoi, save_tiff = TRUE) {
  # Create temporary directory
  tmp <- tempdir()
  
  # Pull basename from url
  basename <- basename(url)
  
  # Download the file to the temporary path
  tryCatch({
    # Takes about 6-7 mins on my internet. Set a timeout of 15 mins here
    options(timeout = 900)
    download.file(url, 
                  destfile = file.path(tmp, basename), 
                  mode = "wb",
                  method = "auto")
    options(timeout = 60)
    cat("Download complete.\n")
  }, error = function(e) {
    cat(paste("Error during download:", e$message, "\n"))
  })
  
  # Unzip and crop to area of interest (aoi)
  if (file.exists(tmp)) {
    # Find the zipped folder and unzip to same location
    unzip(file.path(tmp, basename),
          exdir = file.path(tmp))
    # Find and load the .tif we need
    t <- terra::rast(file.path(tmp, gsub(".zip", ".tif", basename)))
    # Extract `t` CRS
    # t_crs <- terra::crs(t)
    # t_epsg <- stringr::str_extract(t_crs, "EPSG.*$") |> 
    #   stringr::str_extract(pattern = "\\d+") |>
    #   as.numeric()
    # Transform aoi to the same CRS as `t`
    # Do in manually. Can't correctly extract crs programmatically.
    # Had to use the EPSG code from QGIS.
    aoi3978 <- sf::st_transform(aoi, 3978)
    # Crop to aoi
    t <- terra::crop(t, aoi3978)
    t <- terra::mask(t, aoi3978)
    # Transform `t`
    #t <- terra::project(t, "epsg:3005")
    #t <- terra::crop(t, aoi)
    
    # Save raster locally, if applicable
    if (save_tiff) {
      # Create "GIS/Forest Age" dir
      dir.create("GIS/Forest Age", showWarnings = FALSE)
      # Save `t` to "GIS/Forest Age" dir
      terra::writeRaster(t, file.path("GIS/Forest Age", gsub(".zip", ".tiff", basename)), overwrite = TRUE)
      # Move the README txt file to "GIS/Forest Age" dir
      file.copy(file.path(tmp, gsub(".zip", "_README.txt", basename)),
                file.path("GIS/Forest Age", gsub(".zip", "_README.txt", basename)))
    }
    
  }
  
  # Delete the temporary file when no longer needed
  # The 'on.exit()' function is useful for ensuring the file is deleted
  # even if an error occurs within the function where it is used.
  on.exit(unlink(tmp, recursive = TRUE))
  
  # Return t
  return(t)
  
}

calc_disturbance_lyr <- function(res, vri, depletions, retention, forest_age, change_detection) {
  ## Setup ##
  deps <- depletions
  ret <- retention
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
  
  ## Prepare retentions ##
  # Pull out retention areas
  ret <- ret[which(ret$SILV_RESERVE_CODE %in% c("G", "O", "R", "U", "W")),]
  
  # Project stand age to 2022 baseline, so all polygons are aged to 2022
  # Forest Age is projected to 2022 so it will all match
  ret$proj_age <- ret$I_SPECIES_AGE_1 + (2022 - ret$REFERENCE_YEAR)
  
  # Set up forest_age
  # Per documentation, 255 is 'non-forest' area
  # Replace 255 with NA
  forest_age[forest_age > 151] <- NA
  
  # Exact extract MAX AGE for retention patches from forest_age
  # A separate analysis showed max pixel per retention patch lines up
  # best with existing retention patch ages, when available
  ret$forest_age <- exactextractr::exact_extract(forest_age, ret, fun = "max")
  
  # Use provided age of retention (projected to 2022) if it's there, 
  # otherwise for NULL retention areas assign the forest_age.
  ret$age <- ifelse(is.na(ret$proj_age), ret$forest_age, ret$proj_age)
  
  # Finally, convert 'age' into a 'disturbance year'
  ret$disturbance_year <- 2022 - ret$age
  
  ## Prepare forest_age ##
  # 1) Convert forest 'age' to 'disturbance year'
  # 2) reproject forest_age to the correct CRS -> we didn't do this in the
  #     previous section so that we'd get more accurate pixel extraction
  #     (more accurate to warp polygons then extract raster data than the
  #     other way around.)
  
  # Modify from 'age' to 'disturbance year'
  forest_age <- 2022 - forest_age
  
  # Re-project
  # Use method = "near" when re-projecting forest age to new projection,
  # otherwise you get inaccurate smoothing of age. We need to maintain those
  # sharp boundaries delineating forest patches from cuts.
  forest_age <- terra::project(forest_age, "epsg:3005", method = "near")
  
  ## Prepare depletions ##
  # This one is simple. Just make a matching disturbance_year col.
  deps$disturbance_year <- deps$Depletion_Year
  
  ## Rasterize ##
  # First check everything is the same CRS
  same_crs <- all(sf::st_crs(vri) == sf::st_crs(deps),
                  sf::st_crs(deps) == sf::st_crs(cd),
                  sf::st_crs(ret) == sf::st_crs(cd),
                  sf::st_crs(forest_age) == sf::st_crs(ret))
  
  if (!same_crs) stop("Supplied VRI, depletions, retention and change detection have differing CRS.")
  
  # Rasterize VRI + deps
  # Drop NA disturbance year polygons
  vri <- vri[which(!is.na(vri$disturbance_year)), ]
  
  # Figure out max extent that encompasses all 5 layers
  bounds <- c(xmin = min(sf::st_bbox(vri)[1], sf::st_bbox(deps)[1], sf::st_bbox(ret)[1], sf::st_bbox(cd)[1]),
              ymin = min(sf::st_bbox(vri)[2], sf::st_bbox(deps)[2], sf::st_bbox(ret)[2], sf::st_bbox(cd)[2]),
              xmax = max(sf::st_bbox(vri)[3], sf::st_bbox(deps)[3], sf::st_bbox(ret)[3], sf::st_bbox(cd)[3]),
              ymax = max(sf::st_bbox(vri)[4], sf::st_bbox(deps)[4], sf::st_bbox(ret)[4], sf::st_bbox(cd)[4]))
  bounds <- bounds |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_as_sf(crs = sf::st_crs(vri)) |>
    raster::extent() # convert to `raster` pkg type extent object

  # Create a raster template following the supplied resolution
  temp <- raster::raster(bounds, # the extent will be equal to the bounds calculated in `bounds`
                         res = res, # the resolution will be the supplied resolution
                         crs = terra::crs(vri)) # the CRS will be that of VRI (which is that of every other layer)
  
  # Now rasterize
  # VRI
  vri_dist_year <- fasterize::fasterize(vri, temp, field = "disturbance_year")
  vri_dist_year <- terra::rast(vri_dist_year)
  
  # deps
  deps_dist_year <- fasterize::fasterize(deps, temp, field = "disturbance_year")
  deps_dist_year <- terra::rast(deps_dist_year)
  
  # ret
  ret_dist_year <- fasterize::fasterize(ret, temp, field = "disturbance_year")
  ret_dist_year <- terra::rast(ret_dist_year)
  
  # forest_age
  forest_age <- terra::resample(forest_age, vri_dist_year)
  names(forest_age) <- "disturbance_year"
  
  # cd
  cd <- terra::resample(cd, vri_dist_year)
  names(cd) <- "disturbance_year"
  
  # Create our disturbance raster `d` by layering our data together like
  # a fabulous data cake
  # First, between depletions or VRI disturbance year, choose depletions
  d <- terra::merge(deps_dist_year, vri_dist_year, first = TRUE)
  # Next, between above and change detection, choose change detection
  d <- terra::merge(cd, d, first = TRUE)
  # Next, if retention age is available, use that
  d <- terra::merge(ret_dist_year, d, first = TRUE)
  # Finally, merge in forest_age to fill any remaining NA areas
  d <- terra::merge(d, forest_age, first = TRUE)
  
  # Assign correct EPSG code
  terra::crs(d) <- "epsg:3005"
  
  # Return
  return(d)
}


# Extract disturbance layer data (base layer)
extract_disturbance_year <- function(pts, id_col, disturbance) {
  # Set up
  d <- disturbance
  # Subset pts to just ID column
  pts <- pts[,id_col]
  # Extract disturbance
  out <- terra::extract(d, pts, ID = FALSE)
  # Return out
  out <- cbind(pts, out)
  names(out)[2] <- "disturbance_year"
  out <- sf::st_drop_geometry(out)
  return(out)
}

# We can take advantage of slope algorithms to extract stand edges from
# the disturbance layer.
extract_stand_edge <- function(pts, id_col, stand_edge) {
  # Subset pts to just ID column
  pts <- pts[,id_col]
  # Extract "slope"
  out <- terra::extract(stand_edge, pts, ID = FALSE)
  # Return out
  out <- cbind(pts, out)
  names(out)[2] <- "edginess" # for lack of a better term??
  out <- sf::st_drop_geometry(out)
  return(out)
}

extract_edge_dist <- function(pts, id_col, edge_dist) {
  # Subset pts to just ID column
  pts <- pts[,id_col]
  # Extract distance to nearest edge ("slope" > 0)
  out <- terra::extract(edge_dist, pts, ID = FALSE)
  # Return out
  out <- cbind(pts, out)
  names(out)[2] <- "edge_dist_m"
  out <- sf::st_drop_geometry(out)
  return(out)
}


# Wrap up all 3
extract_disturbance <- function(pts, 
                                id_col = "idposition", 
                                disturbance, 
                                stand_edge, 
                                edge_dist) {
  message("Extracting disturbance year...")
  dist_year <- extract_disturbance_year(pts, id_col, disturbance)
  message("Extracting stand edges...")
  edginess <- extract_stand_edge(pts, id_col, stand_edge)
  message("Extracting distance to stand edge...")
  edge_dist_m <- extract_edge_dist(pts, id_col, edge_dist)
  
  # Merge all 4
  out <- merge(dist_year, edginess, all = TRUE)
  out <- merge(out, edge_dist_m, all = TRUE)
  
  return(out)
}

