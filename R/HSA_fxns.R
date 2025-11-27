# Habitat Selection Analysis functions
# These functions are for creating data products related to
# RSFs, SSFs, etc. 

# This fxn creates a shapefile of our study area around our elk.
# First, it pulls BC Natural Earth data to get a course shape
# of Vancouver Island. Then it pulls BC lake data. Combined it creates
# a low res polygon of land areas within our elk study region.
study_area_poly <- function(elk) {
  # First let's get a polygon of Vancouver Island
  # Pull BC shapefile from rnaturalearth
  bc <- rnaturalearth::ne_states(country = "canada")
  bc <- bc[bc$name == "British Columbia", ]
  bc <- sf::st_transform(bc, 3005) # BC Albers projection
  bc  <- sf::st_cast(bc, "POLYGON") # Multipart to Singlepart
  bc$area_sqkm <- sf::st_area(bc) # Recalculate area of each indiv polygon now
  bc$area_sqkm <- units::set_units(bc$area_sqkm, "km2") # Convert unit to sqkm
  # We know Vancouver island is approx 32,000 km2, so subset data to polygons approx that size!
  vi <- bc[bc$area_sqkm > units::set_units(30000, "km2") & bc$area_sqkm < units::set_units(33000, "km2"), ]
  vi <- sf::st_geometry(vi) # drop attributes - keep just the geometry
  
  # Create a minimum convex polygon that contains all our elk
  # data points
  study_area <- sf::st_convex_hull(sf::st_union(elk))
  study_area <- sf::st_buffer(study_area, dist = 10) # buffer by 10km
  study_area <- sf::st_intersection(study_area, vi) # Intersect w VI to cut out water areas
  
  # Lakes
  #bcdata::bcdc_get_record("be394666-5850-4951-8c68-724ae7f72017")
  lakes <- bcdata::bcdc_query_geodata("be394666-5850-4951-8c68-724ae7f72017") |>
    dplyr::filter(WSA_STREAM_ORDER_50K > 3) |>
    dplyr::filter(bcdata::INTERSECTS(study_area)) |>
    dplyr::collect()
  lakes <- sf::st_intersection(lakes, study_area)
  lakes <- sf::st_geometry(lakes)
  
  study_area <- sf::st_difference(study_area, sf::st_union(lakes))
  
  return(study_area)
  
}