# VRI fxns

# Functions to extract the VRI data from the geodatabase file,
# clean it up, extract attributes from the VRI, and extract
# polygon edges from the VRI.

read_vri <- function(gdb, layer = "VEG_COMP_LYR_R1_POLY_CC_RES") {
  vri <- sf::st_read(gdb, layer = layer)
  # Fix the buggy polygons
  vri <- sf::st_cast(vri, "MULTIPOLYGON")
  vri <- sf::st_make_valid(vri)
  # Remove ocean polygons
  vri <- vri[which(vri$BEST_AGE_CL_STS != -1), ]
  # Recalculate shape area
  vri$Shape_Area <- sf::st_area(vri)
  return(vri)
}

extract_vri_edges <- function(elk, vri) {
  # TODO: running out of memory to do this on the whole dataset.
  # Need to think of a more clever way of doing this.
  # Either subset down to only overlapping polygons, or intersect
  # with a mcp that contains all points, then run this thing
  # OR calc distance to non-union'd `out` and choose the
  # minimum value for each one
  # Subset VRI polygons to just the ones overlapping elk
  # GPS points - i.e. thin out the dataset
  # e.g. sf::st_distance(elk, vri)
  t <- sf::st_within(elk, vri, sparse = TRUE)
  t <- unlist(t)
  vri <- vri[t, ]
  #  Convert to linestring
  out <- sf::st_cast(vri, "MULTILINESTRING")
  out <- sf::st_geometry(out)
  out <- sf::st_union(out) # make it all one feature
  return(out)
}



extract_vri <- function(feature, id_col = "idposition",
                        vri, cols) {
  # Data health checks
  stopifnot("`feature` must be a sf class geometry." = inherits(feature, "sf"))
  stopifnot("`feature` must be a sf class POINT geometry." = sf::st_geometry_type(feature) %in% 'POINT')
  stopifnot("`vri` must be a sf class geometry." = inherits(vri, "sf"))
  stopifnot("`vri` must be a sf class MULTIPOLYGON or POLYGON geometry." = any(sf::st_geometry_type(vri) %in% c('MULTIPOLYGON', 'POLYGON')))
  stopifnot("Your VRI layer and feature layer are a different CRS." = sf::st_crs(vri) == sf::st_crs(feature))
  
  
  # If an empty feature was passed to this function (e.g., in a loop), return NA object
  if (nrow(feature) == 0) return(NULL)
  
  # Pare down to cols of interest
  vri <- vri[, cols]
  
  # Ensure name of the geometry column is the same for all features
  #sf::st_geometry(feature) <- "geom"
  #sf::st_geometry(vri) <- "geom"
  
  # Subset feature to just ID column
  feature <- feature[,id_col]
  
  # Intersect with VRI
  ixn <- sf::st_intersection(feature, vri)
  
  ixn <- sf::st_drop_geometry(ixn)
  return(ixn)
}


st_edge_dist <- function(feature, id_col = "idposition", edges) {
  # Data health checks
  stopifnot("`feature` must be a sf class geometry." = inherits(feature, "sf"))
  stopifnot("`feature` must be a sf class POINT geometry." = sf::st_geometry_type(feature) %in% 'POINT')
  stopifnot("`edges` must be a sf class geometry." = inherits(edges, "sf"))
  stopifnot("`edges` must be a sf class MULTILINESTRING or LINESTRING geometry." = any(sf::st_geometry_type(edges) %in% c('MULTILINESTRING', 'LINESTRING')))
  stopifnot("Your edges layer and feature layer are a different CRS." = sf::st_crs(edges) == sf::st_crs(feature))
  
  # Pare down
  feature <- feature[,id_col]
  
  # Extract distances
  out <- sf::st_distance(feature, edges)
  out <- out[,1]
  
  out <- cbind(sf::st_drop_geometry(feature[,id_col]), out)
  
  return(out)
}