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

# Functions to prepare data layers as model inputs for RSF, MaxEnt,
# etc.; functions to prepare prediction dataframes and actually
# predict outputs. 

# PREPARE DATA FOR MODELING FXNS
# Notes:
# This fxn was written to be super generalizable to other potential
# projects - just delete the elk-specific sections of code as needed
# Re weights - 
#   Per Northrup et al. 2022, it's good to 'infinitely' weight your 
#   available obs versus your used obs. 
#   See: Fithian & Hastie (2013) for original work.
#   Muff et al. (2020) indicate a value of 1000 is typically good
#   enough; Northrup suggests 5000.
# Re strings - Any string vars will be interpreted as factors
prepare_mod_dat <- function(presence_pts, # Main presence df data points
                            presence_dat, # Any supplemental data extracted from raster layers you want to merge into main presence df. Must be supplied as a `list()`
                            presence_merge_col = "idposition", # Column to use for merging presence_pts and any supplemental dfs
                            pseudoabsence_pts, # Main psuedoabsence df data points
                            pseudoabsence_dat, # Any supplemental data extracted from raster layers you want to merge into main psuedoabsence df
                            pseudoabsence_merge_col = "idposition", # Column to use for merging pseudoabsence pts and any supplemental dfs
                            id_col = "animal_id", # column to group by/indicator for individual samples
                            min_pts = 100, # minimum sample size of points per `group` that is permitted to keep for modeling
                            pseudoabsence_weight = 5000, # weights value to generate for presences in the 'weights' column
                            pseudoabsence_ratio = 10 # ratio of pseudoabsence pts to presence pts. Defaults to 10:1
                            ) {
  # Convert *_dat obj to lists, if not already (i.e. if only one df supplied)
  if (!(inherits(presence_dat, "list"))) presence_dat <- list(presence_dat)
  if (!(inherits(pseudoabsence_dat, "list"))) pseudoabsence_dat <- list(pseudoabsence_dat)
  
  # Convert all dfs to data.table
  # Presence pts
  presence_pts <- data.table::setDT(presence_pts)
  presence_dat <- lapply(presence_dat, data.table::setDT)
  # Psuedoabsence pts
  pseudoabsence_pts <- data.table::setDT(pseudoabsence_pts)
  pseudoabsence_dat <- lapply(pseudoabsence_dat, data.table::setDT)
  
  # Merge
  presences <- c(list(presence_pts), presence_dat)
  presences <- purrr::reduce(presences, merge, by = presence_merge_col, all.x = TRUE)
  
  pseudoabs <- c(list(pseudoabsence_pts), pseudoabsence_dat)
  pseudoabs <- purrr::reduce(pseudoabs, merge, by = pseudoabsence_merge_col, all.x = TRUE)
  
  ## Presence prep ##
  # Clean column names
  presences <- janitor::clean_names(presences)
  # Drop any geometry, if present
  presences <- sf::st_drop_geometry(presences)
  # Filter to minimum sample size
  presences <- presences |> 
    dplyr::group_by_at(id_col) |>
    dplyr::mutate(N = dplyr::n()) |>
    dplyr::filter(N >= min_pts)
  # Assign presence
  presences$presence <- 1
  # Assign weight
  presences$w <- 1
  
  ## Elk presence-specific prep ## 
  # Remove this section of code if recycling this fxn for other projects
  # Remove any elk where the GPS point occurs BEFORE disturbance 
  # time. We don't know the true forestry conditions of that
  # patch prior to disturbance, so remove them.
  if ("disturbance_year" %in% names(presences)) {
    presences$dist_diff <- presences$dttm - lubridate::date_decimal(presences$disturbance_year)
    presences <- presences[which(presences$dist_diff >= 0), ]
  }
  
  ## Pseudoabsence prep ##
  # Clean names
  pseudoabs <- janitor::clean_names(pseudoabs)
  # Drop any geometry, if present
  pseudoabs <- sf::st_drop_geometry(pseudoabs)
  # Assign presence
  pseudoabs$presence <- 0
  # Assign weight
  pseudoabs$w <- pseudoabsence_weight
  # Subsample pseudoabsences down to the supplied presence:absence ratio
  pseudoabs <- pseudoabs[sample(nrow(pseudoabs), nrow(presences) * pseudoabsence_ratio, replace = FALSE), ]
  # Assign animal_id to random pts
  # For each real presence point (1) in our dataset, we will have a
  # corresponding N `pseudoabsence_ratio` available/unused (0) points in our dataset.
  pseudoabs[[id_col]] <- NA
  pseudoabs[[id_col]] <- rep(presences[[id_col]], pseudoabsence_ratio) # just repeat the animal_id list 10 times
  
  ## Merge the two ##
  dat <- dplyr::bind_rows(presences, pseudoabs)
  
  ## Elk-specific prep ##
  # Remove this section of code if recycling this fxn for other projects
  
  # Create factors
  dat$animal_id <- factor(dat$animal_id)
  dat$season <- factor(dat$season, levels = c("Winter", "Spring", "Summer"))
  
  # Convert degrees to radians
  # This is important bc this var will be wrapped in sine/cosine
  dat$slope_aspect <- dat$slope_aspect * pi / 180
  
  # # Collapse species codes into top X per elk + other
  # # TODO: this might not work for larger samples, e.g. full season datasets
  # if ("species_cd_1" %in% names(dat)) {
  #   # Whatever categories we collapse to, all factor levels must be
  #   # present across all animal_ids, or the models will fail to converge.
  #   # Setting cutoff of must have at least 3 instances of the species
  #   # per animal_id.
  #   ids_in_cd1 <- (table(dat$animal_id, dat$species_cd_1) > 3) |>
  #     as.data.frame() |>
  #     colSums()
  #   # Subset to just those that appear in ALL animal_ids. Everything
  #   # else will be condensed into "other"
  #   ids_in_cd1 <- names(ids_in_cd1[ids_in_cd1 == length(unique(dat$animal_id))])
  #   
  #   dat$species_cd_1 <- ifelse(dat$species_cd_1 %in% ids_in_cd1,
  #                              dat$species_cd_1,
  #                              "Other")
  #   
  #   dat$species_cd_1 <- as.factor(dat$species_cd_1)
  #   
  #   # Make "Western Hemlock" the baseline for spp for more intuitive
  #   # model interpretation. HW is all over the landscape.
  #   dat$species_cd_1 <- forcats::fct_relevel(dat$species_cd_1, "HW")
  # }
  # 
  # if ("species_cd_2" %in% names(dat)) {
  #   ids_in_cd2 <- (table(dat$animal_id, dat$species_cd_2) > 3) |>
  #     as.data.frame() |>
  #     colSums()
  #   ids_in_cd2 <- names(ids_in_cd2[ids_in_cd2 == length(unique(dat$animal_id))])
  #   
  #   dat$species_cd_2 <- ifelse(dat$species_cd_2 %in% ids_in_cd2,
  #                              dat$species_cd_2,
  #                              "Other")
  #   
  #   dat$species_cd_2 <- as.factor(dat$species_cd_2)
  #   
  #   # Make "Western Hemlock" the baseline for spp for more intuitive
  #   # model interpretation. HW is all over the landscape.
  #   dat$species_cd_2 <- forcats::fct_relevel(dat$species_cd_2, "HW")
  # }
  
  # Change it up - let's make this much simpler. 
  # Each tree spp must have at least 1000 obs per group to be 
  # counted as a factor. Otherwise, it's lumped into "Other". 
  if ("species_cd_1" %in% names(dat)) {
    spp1000 <- names(table(dat$species_cd_1))[table(dat$species_cd_1) >= 1000]
    dat$species_cd_1 <- ifelse(dat$species_cd_1 %in% spp1000,
                               dat$species_cd_1,
                               "Other")
    dat$species_cd_1 <- as.factor(dat$species_cd_1)
    # Make "Western Hemlock" the baseline for spp for more intuitive
    # model interpretation. HW is all over the landscape.
    dat$species_cd_1 <- forcats::fct_relevel(dat$species_cd_1, "HW")
  }
  if ("species_cd_2" %in% names(dat)) {
    spp1000 <- names(table(dat$species_cd_2))[table(dat$species_cd_2) >= 1000]
    dat$species_cd_2 <- ifelse(dat$species_cd_2 %in% spp1000,
                               dat$species_cd_2,
                               "Other")
    dat$species_cd_2 <- as.factor(dat$species_cd_2)
    # Make "Western Hemlock" the baseline for spp for more intuitive
    # model interpretation. HW is all over the landscape.
    dat$species_cd_2 <- forcats::fct_relevel(dat$species_cd_2, "HW")
  }
  
  # Reorder, drop cols
  dat <- dat |>
    dplyr::select(idposition, animal_id, collar_id, dttm,
                  presence, w,
                  lat, long, temp_c,
                  year, month, doy, season, 
                  step, angle, nsd, dt, mps, kph,
                  elevation_m:slope_aspect, # DEM attributes
                  proj_age_1:species_cd_3, # VRI attributes
                  disturbance_year:edge_dist_m # disturbance lyr attributes
                  )
  
  dat <- as.data.frame(dat)
  
  
  return(dat)
  
  
}

# Literally the same as the base R `scale()` function, but doesn't
# return the extra attributes that mess up the output.
scale.simple <- function(x) {
  out <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  return(out)
}

# PREPARE DATAFRAME TO PREDICT TO
#prepare_pred_dat <- function(){}


# ACTUALLY PERFORM THE PREDICTIONS
# parallel_predict? Though might just be able to get away w this directly in the pipeline
