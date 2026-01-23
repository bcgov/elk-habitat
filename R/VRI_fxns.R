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



extract_vri <- function(pts, id_col = "idposition",
                        vri, cols) {
  # Data health checks
  stopifnot("`pts` must be a sf class geometry." = inherits(pts, "sf"))
  stopifnot("`pts` must be a sf class POINT geometry." = sf::st_geometry_type(pts) %in% 'POINT')
  stopifnot("`vri` must be a sf class geometry." = inherits(vri, "sf"))
  stopifnot("`vri` must be a sf class MULTIPOLYGON or POLYGON geometry." = any(sf::st_geometry_type(vri) %in% c('MULTIPOLYGON', 'POLYGON')))
  stopifnot("Your VRI layer and pts layer are a different CRS." = sf::st_crs(vri) == sf::st_crs(pts))
  
  
  # If an empty pts was passed to this function (e.g., in a loop), return NA object
  if (nrow(pts) == 0) return(NULL)
  
  # Pare down to cols of interest
  vri <- vri[,cols]
  
  # Ensure name of the geometry column is the same for all ptss
  #sf::st_geometry(pts) <- "geom"
  #sf::st_geometry(vri) <- "geom"

  # Subset pts to just ID column
  pts <- pts[,id_col]
  
  # Intersect with VRI
  ixn <- sf::st_intersection(pts, vri)
  
  # THIS IS IMPORTANT!! 
  # The ORDER that you supply to st_intersection will affect the 
  # row orders of the output. (vri, pts) will output the result 
  # in the same order as `pts`, so you don't need an ID col. You
  # can then run cbind(pts, output) and not worry about mismatched 
  # row order.
  # (pts, vri) will output the result in whatever order the vri
  # data is in. You CANNOT simply then run cbind(pts, output),
  # because output will be arranged according to VRI polygon ID, 
  # NOT pts!
  # Intersect with VRI
  #ixn <- sf::st_intersection(vri, pts)
  
  ixn <- sf::st_drop_geometry(ixn)
  return(ixn)
}

