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



# VRI fxns

# Fxn to extract point data from supplied cols
# The VRI itself is directly pulled from the BCDC in the main pipeline.


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
  
  # Fix coastal douglas fir issue
  # (ALL douglas fir on Vancouver Island is 'coastal' / FDC)
  if ("SPECIES_CD_1" %in% cols) ixn$SPECIES_CD_1 <- ifelse(ixn$SPECIES_CD_1 == "FD", "FDC", ixn$SPECIES_CD_1)
  if ("SPECIES_CD_2" %in% cols) ixn$SPECIES_CD_2 <- ifelse(ixn$SPECIES_CD_2 == "FD", "FDC", ixn$SPECIES_CD_2)
  if ("SPECIES_CD_3" %in% cols) ixn$SPECIES_CD_3 <- ifelse(ixn$SPECIES_CD_3 == "FD", "FDC", ixn$SPECIES_CD_3)
  if ("SPECIES_CD_4" %in% cols) ixn$SPECIES_CD_4 <- ifelse(ixn$SPECIES_CD_4 == "FD", "FDC", ixn$SPECIES_CD_4)
  if ("SPECIES_CD_5" %in% cols) ixn$SPECIES_CD_5 <- ifelse(ixn$SPECIES_CD_5 == "FD", "FDC", ixn$SPECIES_CD_5)
  if ("SPECIES_CD_6" %in% cols) ixn$SPECIES_CD_6 <- ifelse(ixn$SPECIES_CD_6 == "FD", "FDC", ixn$SPECIES_CD_6)
  if ("EST_SITE_INDEX_SPECIES_CD" %in% cols) ixn$EST_SITE_INDEX_SPECIES_CD <- ifelse(ixn$EST_SITE_INDEX_SPECIES_CD == "FD", "FDC", ixn$EST_SITE_INDEX_SPECIES_CD)
  
  return(ixn)
}

