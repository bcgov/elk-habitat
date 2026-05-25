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

# TEM preparation fxns


#' Classify TEM wetland polygons
#' 
#' Pull wetland/riparian polygons from the TEM layer,
#' then sum up the deciles of polygons that match the
#' relevant component codes to get a degree of "wetlandiness"
#' from the TEM polygon.
#'
#' @param TEM TEM polygons, downloaded from the BC Data Catalogue
#' @param wetland_codes Excel spreadsheet from Mario Cottone with all relevant codes
prepare_tem_wetlands <- function(TEM, wetland_codes) {
  # Prepare the wetland codes data
  # Clean names
  wetland_codes <- janitor::clean_names(wetland_codes)
  # In cases where sitemc_s == NA, set it to '0'
  wetland_codes[["sitemc_s"]][is.na(wetland_codes$sitemc_s)] <- "0"
  # In cases where sitemc_s == site_s, set site_s to '00'
  wetland_codes$site_s <- ifelse(wetland_codes$sitemc_s == wetland_codes$site_s, "00", wetland_codes$site_s)
  # Build lookup code!
  wetland_codes$lookup <- paste(wetland_codes$bgc_lbl, wetland_codes$sitemc_s, wetland_codes$site_s, sep = "-")
  
  # Prepare TEM
  tem <- TEM
  
  # Adjust the labels to match those in eco codes table
  tem[["SITE_SERIES_LBL_CPNT_1"]][is.na(tem$SITE_SERIES_LBL_CPNT_1)] <- "00"
  tem[["SITE_SERIES_LBL_CPNT_2"]][is.na(tem$SITE_SERIES_LBL_CPNT_2)] <- "00"
  tem[["SITE_SERIES_LBL_CPNT_3"]][is.na(tem$SITE_SERIES_LBL_CPNT_3)] <- "00"
  
  tem[["SITE_SERIES_MAP_CDE_LBL_CPNT_1"]][is.na(tem$SITE_SERIES_MAP_CDE_LBL_CPNT_1)] <- "0"
  tem[["SITE_SERIES_MAP_CDE_LBL_CPNT_2"]][is.na(tem$SITE_SERIES_MAP_CDE_LBL_CPNT_2)] <- "0"
  tem[["SITE_SERIES_MAP_CDE_LBL_CPNT_3"]][is.na(tem$SITE_SERIES_MAP_CDE_LBL_CPNT_3)] <- "0"
  
  # Build TEM lookup codes - one for each component
  tem$lookup1 <- paste(tem$BIOGEOCLIMATIC_LBL, tem$SITE_SERIES_MAP_CDE_LBL_CPNT_1, tem$SITE_SERIES_LBL_CPNT_1, sep = "-")
  tem$lookup2 <- paste(tem$BIOGEOCLIMATIC_LBL, tem$SITE_SERIES_MAP_CDE_LBL_CPNT_2, tem$SITE_SERIES_LBL_CPNT_2, sep = "-")
  tem$lookup3 <- paste(tem$BIOGEOCLIMATIC_LBL, tem$SITE_SERIES_MAP_CDE_LBL_CPNT_3, tem$SITE_SERIES_LBL_CPNT_3, sep = "-")
  
  # Previous approach - extract wetland-only polyons
  # # Create 3 vectors containing the passing row #s.
  # fltr1 <- which(tem$lookup1 %in% wetland_codes$lookup)
  # fltr2 <- which(tem$lookup2 %in% wetland_codes$lookup)
  # fltr3 <- which(tem$lookup3 %in% wetland_codes$lookup)
  # 
  # fltr <- unique(c(fltr1, fltr2, fltr3))
  # 
  # wetlands <- tem[fltr,]
  # 
  # # Let's get the wetlandiness amount in there
  # # If the component falls within Mario's wetland component list,
  # # add up the deciles. Otherwise, exclude the deciles.
  # wetlands$l1_true <- wetlands$lookup1 %in% wetland_codes$lookup
  # wetlands$l2_true <- wetlands$lookup2 %in% wetland_codes$lookup
  # wetlands$l3_true <- wetlands$lookup3 %in% wetland_codes$lookup
  # 
  # wetlands$wetland1 <- wetlands$l1_true * wetlands$ECOSYSTEM_DECILE_CPNT_1
  # wetlands$wetland2 <- wetlands$l2_true * wetlands$ECOSYSTEM_DECILE_CPNT_2
  # wetlands$wetland3 <- wetlands$l3_true * wetlands$ECOSYSTEM_DECILE_CPNT_2
  # 
  # wetlands$wetland_component <- rowSums(data.frame(wetlands$wetland1, wetlands$wetland2, wetlands$wetland3), na.rm = TRUE)
  
  # Current approach - keep al polygons but label appropriately
  # This way we are aware of 0 wetland vs true no TEM data areas
  tem$l1_true <- tem$lookup1 %in% wetland_codes$lookup
  tem$l2_true <- tem$lookup2 %in% wetland_codes$lookup
  tem$l3_true <- tem$lookup3 %in% wetland_codes$lookup
  
  tem$wetland1 <- tem$l1_true * tem$ECOSYSTEM_DECILE_CPNT_1
  tem$wetland2 <- tem$l2_true * tem$ECOSYSTEM_DECILE_CPNT_2
  tem$wetland3 <- tem$l3_true * tem$ECOSYSTEM_DECILE_CPNT_3
  
  tem$wetland_component <- rowSums(data.frame(tem$wetland1, tem$wetland2, tem$wetland3), na.rm = TRUE)
  
  return(tem)
  
}
