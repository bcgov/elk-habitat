
# DEM fxns
# Functions to download DEM data, load LiDAR data layers, and merge
# elk GPS data to said layers.

#### QUERY CDED ####

# Get the DEM data for HG and VI

query_cded <- function(elk, output_dir) {
  # Draw bounding box around elk
  bbox <- sf::st_bbox(elk)
  # Add 10km buffer to bbox
  bbox[1:2] <- bbox[1:2] - 10000 
  bbox[3:4] <- bbox[3:4] + 10000 
  # Convert vertices into a polygon
  bbox <- st_as_sfc(bbox) |> sf::st_as_sf()
  sf::st_crs(bbox) <- sf::st_crs(elk)
  # Create output directory
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  # Download it
  out <- bcmaps::cded(aoi = bbox,
                      dest_vrt = file.path(output_dir, "CDED_VRT.vrt"))
  return(out)
}


#### PROCESS DEM ####

extract_elevation <- function(elk, cded_path) {
  # Load up CDED VRT
  cded <- terra::rast(cded_path)
  # Extract CRS
  cded_crs <- terra::crs(cded)
  cded_epsg <- stringr::str_extract(cded_crs, "EPSG.*$") |> 
    stringr::str_extract(pattern = "\\d+") |>
    as.numeric()
  # Transform elk data to match DEM CRS
  elk <- sf::st_transform(elk, cded_epsg)
  # Extract elevation
  out <- terra::extract(cded, elk, ID = FALSE)
  # Return out
  out <- cbind(elk$idposition, out)
  names(out) <- c("idposition", "elevation_m")
  return(out)
}


extract_slope <- function(elk, cded_path) {
  # Load up CDED VRT
  cded <- terra::rast(cded_path)
  # Extract CRS
  cded_crs <- terra::crs(cded)
  cded_epsg <- stringr::str_extract(cded_crs, "EPSG.*$") |> 
    stringr::str_extract(pattern = "\\d+") |>
    as.numeric()
  # Transform elk data to match DEM CRS
  elk <- sf::st_transform(elk, cded_epsg)
  # Transform DEM to slope
  slope <- terra::terrain(cded, v = "slope", unit = "radians")
  # Extract slope
  out <- terra::extract(slope, elk, ID = FALSE)
  # Convert from degrees to %
  out$slope_prct <- tan(out$slope)
  # Return out
  out <- cbind(elk$idposition, out)
  names(out) <- c("idposition", "slope_rad", "slope_prct")
  out <- out[,c("idposition", "slope_prct")]
  return(out)
}


extract_aspect <- function(elk, cded_path) {
  # Load up CDED VRT
  cded <- terra::rast(cded_path)
  # Extract CRS
  cded_crs <- terra::crs(cded)
  cded_epsg <- stringr::str_extract(cded_crs, "EPSG.*$") |> 
    stringr::str_extract(pattern = "\\d+") |>
    as.numeric()
  # Transform elk data to match DEM CRS
  elk <- sf::st_transform(elk, cded_epsg)
  # Transform DEM to aspect
  aspect <- terra::terrain(cded, v = "aspect", unit = "degrees")
  # Extract aspect
  out <- terra::extract(aspect, elk, ID = FALSE)
  # Return out
  out <- cbind(elk$idposition, out)
  names(out) <- c("idposition", "slope_aspect")
  return(out)
}


extract_roughness <- function(elk, cded_path) {
  # Load up CDED VRT
  cded <- terra::rast(cded_path)
  # Extract CRS
  cded_crs <- terra::crs(cded)
  cded_epsg <- stringr::str_extract(cded_crs, "EPSG.*$") |> 
    stringr::str_extract(pattern = "\\d+") |>
    as.numeric()
  # Transform elk data to match DEM CRS
  elk <- sf::st_transform(elk, cded_epsg)
  # Transform DEM to roughness
  roughness <- terra::terrain(cded, v = "roughness")
  # Extract aspect
  out <- terra::extract(roughness, elk, ID = FALSE)
  # Return out
  out <- cbind(elk$idposition, out)
  names(out) <- c("idposition", "roughness")
  return(out)
}


# Wrap up all 4
extract_dem <- function(elk, cded_path) {
  message("Extracting elevation...")
  elev <- extract_elevation(elk, cded_path)
  message("Extracting slope...")
  slope <- extract_slope(elk, cded_path)
  message("Extracting aspect...")
  aspect <- extract_aspect(elk, cded_path)
  message("Extracting roughness...")
  roughness <- extract_roughness(elk, cded_path)
  
  # Merge all 3
  out <- merge(elev, slope, all = TRUE)
  out <- merge(out, aspect, all = TRUE)
  out <- merge(out, roughness, all = TRUE)
  
  # Clean up
  out$slope_prct <- round(out$slope_prct * 100, 1)
  out$slope_aspect <- round(out$slope_aspect, 0)
  
  return(out)
}