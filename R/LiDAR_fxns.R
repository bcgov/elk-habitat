# LiDAR fxns

# Functions to download the LiDAR-derived data off the W/L drive,
# then extract the attributes from them.

download_from_server <- function(server_path, local_path, download = TRUE) {
  # Create the local folder where data will be stored, if it
  # doesn't exist already
  dir.create(local_path, showWarnings = FALSE, recursive = TRUE)
  # Create new file path location
  filename <- basename(server_path)
  new_path <- file.path(local_path, filename)
  # Then download it, if download == TRUE
  if (download) {
    fs::file_copy(path = server_path,
                  new_path = new_path,
                  overwrite = TRUE)
  }
  return(new_path)
}


# There are 5 layers in the UWR LiDAR data products:
# 1. canopy_height
# 2. Edge_Category
# 3. Edge_Distance_LiDAR
# 4. elevation
# 5. slope_percent

# Generic function to extract data from any of these
# layers
extract_uwr_lyr <- function(elk, gdb, layer) {
  # Load up the layer
  gdb_dat <- terra::rast(gdb, subds = layer)
  # Extract CRS
  gdb_dat_crs <- terra::crs(gdb_dat)
  gdb_dat_epsg <- stringr::str_extract(gdb_dat_crs, "EPSG.*$") |> 
    stringr::str_extract(pattern = "\\d+") |>
    as.numeric()
  # Transform elk data to match DEM CRS
  elk <- sf::st_transform(elk, gdb_dat_epsg)
  # Extract raster value
  out <- terra::extract(gdb_dat, elk, ID = FALSE)
  # Return out
  out <- cbind(elk$idposition, out)
  names(out)[1] <- "idposition"
  # Pare down to only cols with data
  out <- na.omit(out)
  return(out)
}

# Bundle extracting all of them together
extract_uwr <- function(elk, gdb, layers) {
  # Extract out data from each relevant layer
  uwr <- lapply(layers, function(l) {
    message("Extracting ", l, "...")
    l_out <- extract_uwr_lyr(elk = elk, gdb = gdb, layer = l)
    return(l_out)
  })
  # Assign the datatype as a column name to each df
  names(uwr) <- layers
  invisible(lapply(layers, function(l) {
    uwr[[l]][["layer"]] <<- names(uwr[[l]])[2] # add a column called "layer", whose values consist of the second column name
    names(uwr[[l]])[2] <<- "value" # rename the second column name to "value"
    uwr[[l]][["value"]] <<- as.numeric(uwr[[l]][["value"]]) # convert the data in "value" to be numeric data type
  }))
  # Merge all the uwr dfs into a single df
  uwr <- dplyr::bind_rows(uwr)
  # Widen
  uwr <- tidyr::pivot_wider(uwr, names_from = "layer", values_from = "value")
  return(uwr)
}


# Extract from Crown Height Model raster directly, rather than the UWR GDB
extract_chm <- function(elk, path) {
  message("Extracting crown height model data...")
  # Load up the raster
  chm <- terra::rast(path)
  # Extract CRS
  dat_crs <- terra::crs(chm)
  dat_epsg <- stringr::str_extract(dat_crs, "EPSG.*$") |> 
    stringr::str_extract(pattern = "\\d+") |>
    as.numeric()
  # Transform elk data to match DEM CRS
  elk <- sf::st_transform(elk, dat_epsg)
  # Extract raster value
  out <- terra::extract(chm, elk, ID = FALSE)
  # Return out
  out <- cbind(elk$idposition, out)
  names(out)[1] <- "idposition"
  # Pare down to only cols with data
  out <- na.omit(out)
  return(out)
}

