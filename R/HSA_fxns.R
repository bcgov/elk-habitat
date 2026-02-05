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


# Habitat Selection Analysis functions
# These functions are for creating data products related to
# RSFs, SSFs, etc. 

# This fxn creates a shapefile of our study area around our elk.
# First, it pulls BC Natural Earth data to get a course shape
# of Vancouver Island. Then it pulls BC lake data. Combined it creates
# a low res polygon of land areas within our elk study region.
study_area_poly <- function(elk, cded_path) {
  
  # First let's get a polygon of land in our study area, using the DEM
  # Load up our DEM
  DEM <- terra::rast(cded_path)
  DEM <- DEM > 0 # land vs not land
  DEM <- terra::as.polygons(DEM) # polygonize
  DEM <- sf::st_as_sf(DEM) # convert to sf obj
  DEM <- DEM[DEM$CDED_VRT == 1, ] # throw out ocean area
  DEM <- sf::st_geometry(DEM) # drop attributes - keep just the geometry
  vi <- sf::st_as_sf(DEM) # merge into one polygon feature called 'vi'
  # clip that little weird feature on the north end
  clippy <- data.frame(id = c(1, 1),
                       lon = c(-126.0002367387105551, -125.9997585425571884),
                       lat = c(50.4186361717511602, 50.4184272150094941)) |>
    sf::st_as_sf(coords = c("lon", "lat"), na.fail = FALSE, crs = "epsg:4269") |> 
    dplyr::group_by(id) |> 
    dplyr::summarize() |>
    st_cast("LINESTRING")
  vi <- lwgeom::st_split(vi, clippy) |> sf::st_collection_extract("POLYGON")
  # Assign polygon IDs
  vi$id <- as.numeric(row.names(vi))
  vi$area <- sf::st_area(vi)
  # Inspect 
  #mapview::mapview(vi, zcol = "area") + mapview::mapview(clippy)
  # Now extract centroids of our polygons. We'll filter out anything
  # north of the main VI landmass
  vi <- cbind(vi, sf::st_coordinates(suppressWarnings(sf::st_centroid(vi))))
  # Extract the latitude of the largest polygon. anything north of
  # that will be removed
  max_lat <- vi[["Y"]][vi$area == max(vi$area)]
  # Inspect
  # plot(sf::st_geometry(vi))
  # plot(sf::st_geometry(vi[vi$Y <= max_lat, ]))
  # Apply filter
  vi <- vi[vi$Y <= max_lat, ]
  # Merge into single polygon, and keep only geometry
  vi <- dplyr::summarise(vi) |> sf::st_geometry()
  # Reproject
  vi <- sf::st_transform(vi, 3005)
  # Remove small holes in the polygon, caused by DEM errors
  vi <- nngeo::st_remove_holes(vi, max_area = 100000)
  
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
  lakes <- suppressWarnings(sf::st_intersection(lakes, study_area))
  lakes <- sf::st_geometry(lakes)
  
  study_area <- sf::st_difference(study_area, sf::st_union(lakes))
  study_area <- sf::st_as_sf(study_area)
  
  return(study_area)
  
}