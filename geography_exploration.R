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

# Description

# This script generates visuals for our variables of interest that will
# be included in our habitat selection models. They are intended to 
# visualize use (selection) vs availability of various landscape features.

# TODO: replace the study area here with the targets pipeline study area
# for pulling random availability points from.

# SETUP -----------------------------------------------------------------

# Load libraries
library(targets)
library(sf)
library(ggplot2)
library(ggsignif)
library(ggmosaic)
library(DataExplorer)
library(units)

# Color palette
okabe <- c("#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

# Set static variables

# Severe winter period was 18 Dec 2021 - 14 Jan 2022
#lubridate::isoweek("2021-12-18")
#lubridate::isoweek("2022-01-14")
swp_weeks <- c(50, 51, 52, 53, 1, 2) # weeks 51 thru 1

#lubridate::yday("2021-12-18")
#lubridate::yday("2022-01-14")
swp_dates <- seq(lubridate::ymd("2021-12-18"), lubridate::ymd("2022-01-14"), by = "1 day")
swp_days <- c(lubridate::yday("2021-12-18"):366, 1:14)

## Elk ####
# Load up targets
tar_load(elk)
tar_load(elk_dem) # TRIM DEM-derived attributes
elk_lidar <- tar_read(elk_uwr) # UWR LiDAR-derived attributes
tar_load(elk_chm) # BCTS Crown Height Model
tar_load(elk_vri) # VRI-derived attributes
tar_load(elk_disturbance) # Disturbance layer attributes

# Two elk did not experience severe winter conditions, per Mario's
# work looking at snow depth data on cameras deployed across the
# study region. Remove these two elk from the severe winter
# comparison.
non_swp_elk <- c("20-1001", "20-1002")

# These are the elk IDs that specifically experienced the 2021
# severe winter (i.e. cuts out any that also weren't collared
# yet)
swp_elk <- unique(elk[["animal_id"]][lubridate::date(elk$dttm) %in% swp_dates])

# Set up SWP flag
elk$SWP <- FALSE
elk[["SWP"]][which(elk$animal_id %in% swp_elk & lubridate::yday(elk$dttm) %in% swp_days)] <- TRUE

# Set up other date cols
elk$year <- lubridate::year(elk$dttm)
elk$yday <- lubridate::yday(elk$dttm)

# Merge extracted raster values
elk <- janitor::remove_empty(elk)
elk <- merge(elk, elk_dem, by = "idposition", all = TRUE)
elk <- merge(elk, elk_lidar, by = "idposition", all = TRUE)
elk <- merge(elk, elk_chm, by = "idposition", all = TRUE)
elk <- merge(elk, elk_vri, by = "idposition", all = TRUE)
elk <- merge(elk, elk_disturbance, by = "idposition", all = TRUE)

rm(elk_dem, elk_lidar, elk_chm, elk_vri, elk_disturbance)

elk <- janitor::clean_names(elk)

names(elk)[grep("li_dar", names(elk))] <- "edge_dist"


#' These are our newly added variables of interest:
#' GPS VARIABLES
#' - elev_m
#' - temp_c
#' DEM VARIABLES
#' - elevation_m
#' - slope_prct
#' - slope_aspect
#' - roughness
#' LIDAR VARIABLES
#' - canopy_height
#' - edge_category
#' - edge_distance_lidar
#' - elevation
#' - slope_percent
#' CHM VARIABLES
#' - crown_height
#' VRI VARIABLES
#' - projected_date - to flag any GPS points that come after that as useless data
#' - harvest_date
#' - harvest_year - to flag any GPS points that might confound a model - that patch may have been forested at the time the elk were there, if harvest date is not known
#' - shape_area - elk patch size use
#' - new_vri_cc_res_age
#' - best_age_cl_sts
#' - canopy height
#' - shrub_height
#' - shrub_crown_closure
#' DISTURBANCE VARIABLES
#' - disturbance_year
#' - edginess - was an elk within a patch (closer to 0) or within an edge (closer to 90)
#' - edge_dist_m

#' Perhaps let's start with comparing the GPS collar 
#' derived elevation to the DEM elevation to the LiDAR
#' elevation (which is probably the most accurate)


# Histogram of all continuous vars
#plot_histogram(elk)



## Study Area ####
tar_load(study_area)

## Random Points ####
# In previous version of this script, random points were
# generated here. Now, random points are generated in the
# targets pipeline, and variables for the random pts are
# extracted in the targets pipeline as well.

# Now, load up the extracted values, and merge them into
# a single `random_pts` df.

## Winter random pts ##
tar_load(random_winter_dem)
tar_load(random_winter_uwr)
tar_load(random_winter_chm)
tar_load(random_winter_vri)
tar_load(random_winter_disturbance)

random_winter <- merge(random_winter_dem, random_winter_uwr, by = "idposition", all = TRUE)
random_winter <- merge(random_winter, random_winter_chm, by = "idposition", all = TRUE)
random_winter <- merge(random_winter, random_winter_vri, by = "idposition", all = TRUE)
random_winter <- merge(random_winter, random_winter_disturbance, by = "idposition", all = TRUE)

random_winter$season <- "Winter"

rm(random_winter_dem, random_winter_uwr, random_winter_chm, random_winter_vri, random_winter_disturbance)

# Subsample to 100k rows so plots don't take forever
random_winter <- random_winter[sample(nrow(random_winter), 100000, replace = FALSE), ]

## Spring random pts ##
tar_load(random_spring_dem)
tar_load(random_spring_uwr)
tar_load(random_spring_chm)
tar_load(random_spring_vri)
tar_load(random_spring_disturbance)

random_spring <- merge(random_spring_dem, random_spring_uwr, by = "idposition", all = TRUE)
random_spring <- merge(random_spring, random_spring_chm, by = "idposition", all = TRUE)
random_spring <- merge(random_spring, random_spring_vri, by = "idposition", all = TRUE)
random_spring <- merge(random_spring, random_spring_disturbance, by = "idposition", all = TRUE)

random_spring$season <- "Spring"

rm(random_spring_dem, random_spring_uwr, random_spring_chm, random_spring_vri, random_spring_disturbance)

# Subsample to 100k rows so plots don't take forever
random_spring <- random_spring[sample(nrow(random_spring), 100000, replace = FALSE), ]

## Summer random pts ##
tar_load(random_summer_dem)
tar_load(random_summer_uwr)
tar_load(random_summer_chm)
tar_load(random_summer_vri)
tar_load(random_summer_disturbance)

random_summer <- merge(random_summer_dem, random_summer_uwr, by = "idposition", all = TRUE)
random_summer <- merge(random_summer, random_summer_chm, by = "idposition", all = TRUE)
random_summer <- merge(random_summer, random_summer_vri, by = "idposition", all = TRUE)
random_summer <- merge(random_summer, random_summer_disturbance, by = "idposition", all = TRUE)

random_summer$season <- "Summer"

rm(random_summer_dem, random_summer_uwr, random_summer_chm, random_summer_vri, random_summer_disturbance)

# Subsample to 100k rows so plots don't take forever
random_summer <- random_summer[sample(nrow(random_summer), 100000, replace = FALSE), ]

## SWP random pts ##
tar_load(random_swp_dem)
tar_load(random_swp_uwr)
tar_load(random_swp_chm)
tar_load(random_swp_vri)
tar_load(random_swp_disturbance)

random_swp <- merge(random_swp_dem, random_swp_uwr, by = "idposition", all = TRUE)
random_swp <- merge(random_swp, random_swp_chm, by = "idposition", all = TRUE)
random_swp <- merge(random_swp, random_swp_vri, by = "idposition", all = TRUE)
random_swp <- merge(random_swp, random_swp_disturbance, by = "idposition", all = TRUE)

# Set SWP flag
random_swp$SWP <- TRUE

rm(random_swp_dem, random_swp_uwr, random_swp_chm, random_swp_vri, random_swp_disturbance)

# Subsample to 50k rows so plots don't take forever
random_swp <- random_swp[sample(nrow(random_swp), 50000, replace = FALSE), ]

# For SWP specifically, randomly assign a yday to a SWP yday
# for plotting purposes later
random_swp$yday <- sample(swp_days, nrow(random_swp), replace = TRUE)


## Merge random pts together ####

random_pts <- dplyr::bind_rows(random_winter, random_spring, random_summer, random_swp)
random_pts <- dplyr::select(random_pts, idposition, season, yday, dplyr::everything())

rm(random_winter, random_spring, random_summer, random_swp)


## Misc fixes ####

# Any "FD" should be "FDC"
elk$species_cd_1 <- ifelse(elk$species_cd_1 == "FD", "FDC", elk$species_cd_1)
elk$species_cd_2 <- ifelse(elk$species_cd_2 == "FD", "FDC", elk$species_cd_2)
elk$species_cd_3 <- ifelse(elk$species_cd_3 == "FD", "FDC", elk$species_cd_3)

random_pts$SPECIES_CD_1 <- ifelse(random_pts$SPECIES_CD_1 == "FD", "FDC", random_pts$SPECIES_CD_1)
random_pts$SPECIES_CD_2 <- ifelse(random_pts$SPECIES_CD_2 == "FD", "FDC", random_pts$SPECIES_CD_2)
random_pts$SPECIES_CD_3 <- ifelse(random_pts$SPECIES_CD_3 == "FD", "FDC", random_pts$SPECIES_CD_3)

# SOURCE COMPARISON -------------------------------------------------------



# Comparison density plots

## Elevation ####
# Without LiDAR
elk |>
  sf::st_drop_geometry() |>
  dplyr::select(elev_m, elevation_m) |>
  dplyr::rename(elevation_gps = elev_m,
                elevation_dem = elevation_m) |>
  na.omit() |>
  tidyr::pivot_longer(cols = 1:2,
                      names_to = "Source",
                      values_to = "m") |>
  ggplot(aes(x = m)) +
  geom_density(aes(color = Source,
                   fill = Source),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Source",
                    labels = c("TRIM DEM",
                               "Collar GPS")) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Source",
                     labels = c("TRIM DEM",
                                "Collar GPS")) +
  scale_x_continuous(limits = c(-10, 2000)) +
  labs(title = "How does the elevation data from various sources compare?",
       x = "Elevation (m)",
       y = "Density") +
  theme_minimal()

# With LiDAR
elk |>
  sf::st_drop_geometry() |>
  dplyr::select(elev_m, elevation_m, elevation) |>
  dplyr::rename(elevation_gps = elev_m,
                elevation_dem = elevation_m,
                elevation_lidar = elevation) |>
  na.omit() |>
  tidyr::pivot_longer(cols = 1:3,
                      names_to = "Source",
                      values_to = "m") |>
  ggplot(aes(x = m)) +
  geom_density(aes(color = Source,
                   fill = Source),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Source",
                     labels = c("TRIM DEM",
                                "Collar GPS",
                                "LiDAR")) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Source",
                     labels = c("TRIM DEM",
                                "Collar GPS",
                                "LiDAR")) +
  scale_x_continuous(limits = c(-10, 2000)) +
  labs(title = "How does the elevation data from various sources compare?",
       x = "Elevation (m)",
       y = "Density") +
  theme_minimal()


## Slope % ####

# All data points
elk |>
  sf::st_drop_geometry() |>
  dplyr::select(slope_prct, slope_percent) |>
  dplyr::rename(slope_dem = slope_prct) |>
  na.omit() |>
  tidyr::pivot_longer(cols = 1:2,
                      names_to = "Source",
                      values_to = "Percent") |>
  ggplot(aes(x = Percent)) +
  geom_density(aes(color = Source,
                   fill = Source),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Source",
                    labels = c("TRIM DEM", "LiDAR")) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Source",
                     labels = c("TRIM DEM", "LiDAR")) +
  labs(title = "How does the slope grade data from various sources compare?",
       x = "Slope grade (%)",
       y = "Density") +
  theme_minimal()



## Crown Height ####
elk |>
  sf::st_drop_geometry() |>
  dplyr::select(crown_height, proj_height_1) |>
  dplyr::rename(ch_vri = proj_height_1,
                ch_lidar = crown_height) |>
  na.omit() |>
  tidyr::pivot_longer(cols = 1:2,
                      names_to = "Source",
                      values_to = "Percent") |>
  ggplot(aes(x = Percent)) +
  geom_density(aes(color = Source,
                   fill = Source),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Source",
                    labels = c("LiDAR Crown Height",
                               "VRI Stand Height")
                    ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Source",
                     labels = c("LiDAR Crown Height",
                                "VRI Stand Height")
                     ) +
  scale_x_continuous(limits = c(-10, 200)) +
  labs(title = "How does the crown/stand height data from various sources compare?",
       x = "Crown Height (m)",
       y = "Density") +
  theme_minimal()



# DEM ---------------------------------------------------------------


## Elevation ####

# Available versus what elk chose

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$elevation_m,
                      "type" = "Random elevations",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP),
               data.frame("val" = elk$elevation_m,
                          "type" = "Elk elevations",
                          "season" = elk$season,
                          "pool" = "real",
                          "dttm" = elk$dttm,
                          "year" = elk$year,
                          "yday" = elk$yday,
                          "swp" = elk$SWP))

p |>
  ggplot(aes(x = val)) +
  geom_density(aes(color = type,
                   fill = type),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Type",
                    #labels = c()
                    ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Type",
                     #labels = c()
                     ) +
  labs(title = "Elevation - available vs elk",
       subtitle = "All dates",
       x = "Elevation (m)",
       y = "Density",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(season %in% c("Winter", "Spring", "Summer")) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              pch = ".",
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk elevations", "Random elevations")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 2500) +
  geom_text(aes(label = N),
            nudge_y = 100) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Elevations (m)") +
  theme_minimal()


## DENSITY PLOT
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(pool %in% c("real")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter"))) |>
  ggplot(aes(x = val,
           color = season,
           fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Elevations (m)",
       y = "Density") +
  theme_minimal()

## DENSITY PLOT SPLIT BY SEASON
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season,
             linetype = type)) +
  geom_density(alpha = 0.2) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  facet_wrap(~ season) +
  labs(x = "Elevations (m)",
       y = "Density",
       caption = "100k random data points for each season are sampled from that season's MCP. \nSeasonal MCPs are derived by merging all the MCPs from each elk-season into a single polygon.") +
  theme_minimal()



# SWP plots

## BOXPLOT - Yearly variation WITHOUT random
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Elevation - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Elevation (m)",
       caption = "2021-2022 was the 'Severe' year.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 1250,
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 1350,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 1500,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 1600,
              color = "black") +
  theme_minimal()

## BOXPLOT - Yearly variation WITH RANDOM
p |>
  dplyr::filter(swp == TRUE) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(year = dplyr::if_else(pool == "random", "Random", year)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:6],
                     name = "Type") +
  labs(title = "Elevation - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Elevation (m)",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 1250,
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 1350,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 1500,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 1600,
              color = "black") +
  theme_minimal()

## DENSITY PLOT
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = val,
             color = as.factor(year))) +
  geom_density(aes(color = year,
                   fill = year),
               alpha = 0.1) +
  scale_fill_manual(values = okabe[1:5],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:5],
                     name = "Year",
                     #labels = c()
  ) +
  labs(title = "Elevation - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Elevation (m)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()


## SWP ACTUAL VS RANDOM
# Random SWP points are sampled from the the pooled Winter MCPs
# of elk individuals that have experienced a severe winter.
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  #units::drop_units() |>
  ggplot() +
  geom_density(aes(x = val,
                   color = year,
                   fill = year,
                   linetype = type),
               alpha = 0.1) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_fill_manual(values = okabe[1:7],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:7],
                     name = "Year",
                     #labels = c()
  ) +
  facet_wrap(~ year) +
  labs(title = "Elevation - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Elevation (m)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()



## Slope % -----------------------------------------------------------------

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$slope_prct,
                      "type" = "Random slopes",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP),
           data.frame("val" = elk$slope_prct,
                      "type" = "Elk slopes",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP))

p <- p[p$val > 0, ] # cut out water (perfectly flat)

# DENSITY PLOT - Random vs Selected
p |>
  ggplot(aes(x = val)) +
  geom_density(aes(color = type,
                   fill = type),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Type",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Type",
                     #labels = c()
  ) +
  labs(title = "Slope grade - available vs elk",
       subtitle = "All dates",
       x = "Slope (%)",
       y = "Density",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(season %in% c("Winter", "Spring", "Summer")) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              pch = ".",
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk slopes", "Random slopes")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 570) +
  geom_text(aes(label = N),
            nudge_y = 10) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Slope (%)",
       caption = "Differing sample sizes within seasons are due removing any random points that fell on water (where slope == 0).") +
  theme_minimal()


## DENSITY PLOT
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = dplyr::if_else(type == "Elk slopes",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Slope (%)",
       y = "Density") +
  theme_minimal()

## DENSITY PLOT SPLIT BY SEASON
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season,
             linetype = type)) +
  geom_density(alpha = 0.2) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  facet_wrap(~ season) +
  labs(x = "Slope (%)",
       y = "Density",
       caption = "100k random data points for each season are sampled from that season's MCP. \nSeasonal MCPs are derived by merging all the MCPs from each elk-season into a single polygon.") +
  theme_minimal()



# SWP plots

## BOXPLOT - Yearly variation WITHOUT random
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Slope - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Slope (%)",
       caption = "2021-2022 was the 'Severe' year.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 230,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 260,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 290,
              color = "black") +
  theme_minimal()


## BOXPLOT - Yearly variation WITH RANDOM
p |>
  dplyr::filter(swp == TRUE) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(year = dplyr::if_else(pool == "random", "Random", year)) |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:6],
                     name = "Type") +
  labs(title = "Slope - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Slope (%)",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 230,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 260,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 290,
              color = "black") +
  theme_minimal()



## DENSITY PLOT
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = val,
             color = as.factor(year))) +
  geom_density(aes(color = year,
                   fill = year),
               alpha = 0.1) +
  scale_fill_manual(values = okabe[1:5],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:5],
                     name = "Year",
                     #labels = c()
  ) +
  labs(title = "Slope - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Slope (%)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()



## SWP ACTUAL VS RANDOM
# Random SWP points are sampled from the the pooled Winter MCPs
# of elk individuals that have experienced a severe winter.
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  #units::drop_units() |>
  ggplot() +
  geom_density(aes(x = val,
                   color = year,
                   fill = year,
                   linetype = type),
               alpha = 0.1) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_fill_manual(values = okabe[1:7],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:7],
                     name = "Year",
                     #labels = c()
  ) +
  facet_wrap(~ year) +
  labs(title = "Slope - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Slope (%)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()


## Slope Aspect ------------------------------------------------------------


# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$slope_aspect,
                      "type" = "Random aspects",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP,
                      "slope" = random_pts$slope_prct),
           data.frame("val" = elk$slope_aspect,
                      "type" = "Elk aspects",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP,
                      "slope" = elk$slope_prct))

# Just like with slope %, cut out any points where slope == 0, i.e. flat areas.
# These greatly skewed the aspect plots with many outlier values at 90 deg aspect.
p <- p[p$slope > 0, ] # cut out water (perfectly flat)

# DENSITY PLOT - Random vs Selected
p |>
  ggplot(aes(x = val)) +
  geom_density(aes(color = type,
                   fill = type),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Type",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Type",
                     #labels = c()
  ) +
  labs(title = "Slope aspect - available vs elk",
       subtitle = "All dates",
       x = "Slope aspect (degrees)",
       y = "Density",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# POLAR histogram
p |>
  ggplot(aes(x = val)) +
  geom_density(aes(color = type,
                   fill = type),
               alpha = 0.1,
               trim = TRUE) +
  # geom_histogram(binwidth = 10,
  #                center = 0) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Type") +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Type") +
  scale_x_continuous(breaks = seq(0, 360, 45)) +
  coord_polar() +
  labs(title = "Slope aspect - available vs elk",
       subtitle = "All dates",
       x = "Slope aspect (degrees)",
       y = "Density",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(season %in% c("Winter", "Spring", "Summer")) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              pch = ".",
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk aspects", "Random aspects")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 370) +
  geom_text(aes(label = N),
            nudge_y = 10) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Slope aspect (degrees)",
       caption = "Differing sample sizes within seasons are due removing any random points that fell on water (where slope == 0).") +
  theme_minimal()


## DENSITY PLOT
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = dplyr::if_else(pool == "real",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Slope aspect (degrees)",
       y = "Density") +
  theme_minimal()

# POLAR density
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = dplyr::if_else(pool == "real",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  scale_x_continuous(breaks = seq(0, 360, 45)) +
  coord_polar() +
  labs(x = "Slope aspect (degrees)",
       y = "Density") +
  theme_minimal()

## DENSITY PLOT SPLIT BY SEASON
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season,
             linetype = type)) +
  geom_density(alpha = 0.2) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  facet_wrap(~ season) +
  labs(x = "Slope aspect (degrees)",
       y = "Density",
       caption = "100k random data points for each season are sampled from that season's MCP. \nSeasonal MCPs are derived by merging all the MCPs from each elk-season into a single polygon.") +
  theme_minimal()


p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season,
             linetype = type)) +
  geom_density(alpha = 0.2) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  scale_x_continuous(breaks = seq(0, 360, 45)) +
  coord_polar() +
  facet_wrap(~ season) +
  labs(x = "Slope aspect (degrees)",
       y = "Density",
       caption = "100k random data points for each season are sampled from that season's MCP. \nSeasonal MCPs are derived by merging all the MCPs from each elk-season into a single polygon.") +
  theme_minimal()


# SWP plots

## BOXPLOT - Yearly variation WITHOUT random
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Slope aspects - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Slope aspect (degrees)",
       caption = "2021-2022 was the 'Severe' year.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 380,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 420,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 440,
              color = "black") +
  theme_minimal()


## BOXPLOT - Yearly variation WITH RANDOM
p |>
  dplyr::filter(swp == TRUE) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(year = dplyr::if_else(pool == "random", "Random", year)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:6],
                     name = "Type") +
  labs(title = "Slope aspects - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Slope aspect (degrees)",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 380,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 420,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 440,
              color = "black") +
  theme_minimal()

## DENSITY PLOT
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = val,
             color = as.factor(year))) +
  geom_density(aes(color = year,
                   fill = year),
               alpha = 0.1) +
  scale_fill_manual(values = okabe[1:5],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:5],
                     name = "Year",
                     #labels = c()
  ) +
  labs(title = "Slope aspect - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Slope aspect (degrees)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()


p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = val,
             color = as.factor(year))) +
  geom_density(aes(color = year,
                   fill = year),
               alpha = 0.1) +
  scale_fill_manual(values = okabe[1:5],
                    name = "Year") +
  scale_color_manual(values = okabe[1:5],
                     name = "Year") +
  scale_x_continuous(breaks = seq(0, 360, 45)) +
  coord_polar() +
  labs(title = "Slope aspect - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Slope aspect (degrees)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()


## SWP ACTUAL VS RANDOM
# Random SWP points are sampled from the the pooled Winter MCPs
# of elk individuals that have experienced a severe winter.
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  #units::drop_units() |>
  ggplot() +
  geom_density(aes(x = val,
                   color = year,
                   fill = year,
                   linetype = type),
               alpha = 0.1) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_fill_manual(values = okabe[1:7],
                    name = "Year") +
  scale_color_manual(values = okabe[1:7],
                     name = "Year") +
  facet_wrap(~ year) +
  labs(title = "Slope aspect - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Slope aspect (degrees)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()



p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  #units::drop_units() |>
  ggplot() +
  geom_density(aes(x = val,
                   color = year,
                   fill = year,
                   linetype = type),
               alpha = 0.1) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_fill_manual(values = okabe[1:7],
                    name = "Year") +
  scale_color_manual(values = okabe[1:7],
                     name = "Year") +
  scale_x_continuous(breaks = seq(0, 360, 45)) +
  coord_polar() +
  facet_wrap(~ year) +
  labs(title = "Slope aspect - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Slope aspect (degrees)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()


## Roughness ---------------------------------------------------------------

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$roughness,
                      "type" = "Random roughness",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP,
                      "slope" = random_pts$slope_prct),
           data.frame("val" = elk$roughness,
                      "type" = "Elk roughness",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP,
                      "slope" = elk$slope_prct))

# Just like with slope %, cut out any points where slope == 0, i.e. flat areas.
# These greatly skewed the aspect plots with many outlier values at 90 deg aspect.
p <- p[p$slope > 0, ] # cut out water (perfectly flat)

# DENSITY PLOT - Random vs Selected
ggplot(p,
       aes(x = val)) +
  geom_density(aes(color = type,
                   fill = type),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Type",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Type",
                     #labels = c()
  ) +
  labs(title = "Roughness - available vs elk",
       subtitle = "All dates",
       x = "Roughness",
       y = "Density",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(season %in% c("Winter", "Spring", "Summer")) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              pch = ".",
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk roughness", "Random roughness")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 175) +
  geom_text(aes(label = N),
            nudge_y = 10) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Roughness",
       caption = "Differing sample sizes within seasons are due removing any random points that fell on water (where slope == 0).") +
  theme_minimal()


## DENSITY PLOT
p |>
  dplyr::mutate(season = dplyr::if_else(pool == "real",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  dplyr::filter(!is.na(season)) |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Roughness",
       y = "Density") +
  theme_minimal()



# SWP plots

## BOXPLOT - Yearly variation
p |>
  dplyr::filter(pool == "real") |>
  dplyr::mutate(yday = lubridate::yday(dttm),
                year = lubridate::year(dttm)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val,
             color = as.factor(year))) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(fill = NA,
               color = "black") +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Roughness - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Roughness",
       caption = "2021-2022 was the 'Severe' year.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 170,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 200,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 230,
              color = "black") +
  theme_minimal()

## DENSITY PLOT
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = val,
             color = as.factor(year))) +
  geom_density(aes(color = year,
                   fill = year),
               alpha = 0.1) +
  scale_fill_manual(values = okabe[1:5],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:5],
                     name = "Year",
                     #labels = c()
  ) +
  labs(title = "Roughness - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Roughness",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()




# # LiDAR -----------------------------------------------------------
# 
# # Not using this data, so skip these plots
# 
# ## UWR Data dates ####
# 
# elk |>
#   dplyr::filter(!is.na(canopy_height)) |>
#   ggplot(aes(x = dttm, y = TRUE)) +
#   geom_point() +
#   theme_minimal()
# 
# ## Canopy Height ####
# 
# uwr <- terra::rast(tar_read(uwr_lidar_gdb), subds = "canopy_height")
# uwr <- terra::values(uwr)
# uwr <- as.data.frame(uwr)
# uwr$type <- "All canopy heights"
# names(uwr)[1] <- "canopy_height"
# 
# uwr_p <- rbind(uwr, 
#                data.frame("canopy_height" = elk$canopy_height,
#                           "type" = "Elk canopy heights"))
# uwr_p <- na.omit(uwr_p)
# 
# 
# ggplot(uwr_p,
#        aes(x = canopy_height)) +
#   geom_density(aes(color = type,
#                    fill = type),
#                alpha = 0.1) +
#   scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                     name = "Type",
#                     #labels = c()
#   ) +
#   scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                      name = "Type",
#                      #labels = c()
#   ) +
#   labs(title = "Canopy height - available vs elk",
#        subtitle = "All dates",
#        x = "Canopy height (m)",
#        y = "Density") +
#   theme_minimal()
# 
# 
# uwr_p <- rbind(uwr, 
#                data.frame("canopy_height" = elk[["canopy_height"]][lubridate::day(elk$dttm) %in% swp_days],
#                           "type" = "Elk canopy heights"))
# 
# ggplot(uwr_p,
#        aes(x = canopy_height)) +
#   geom_density(aes(color = type,
#                    fill = type),
#                alpha = 0.1) +
#   scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                     name = "Type",
#                     #labels = c()
#   ) +
#   scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                      name = "Type",
#                      #labels = c()
#   ) +
#   labs(title = "Canopy height - available vs elk",
#        subtitle = "During the Severe Winter Period",
#        x = "Canopy height (m)",
#        y = "Density") +
#   theme_minimal()
# 
# 
# swp_vs_all <- elk[,c("dttm", "canopy_height")]
# swp_vs_all$in_swp <- lubridate::day(swp_vs_all$dttm) %in% swp_days
# 
# ggplot(swp_vs_all,
#        aes(x = canopy_height)) +
#   geom_density(aes(color = in_swp,
#                    fill = in_swp),
#                alpha = 0.1) +
#   scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                     name = "In SWP?",
#                     #labels = c()
#   ) +
#   scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                      name = "In SWP?",
#                      #labels = c()
#   ) +
#   labs(title = "Canopy height - Non-SWP vs SWP",
#        x = "Canopy height (m)",
#        y = "Density") +
#   theme_minimal()
# 
# 
# ## Edge Category -----------------------------------------------------------
# 
# # TODO: SHOULD ACTUALLY BE A CHI SQUARE
# 
# uwr <- terra::rast(tar_read(uwr_lidar_gdb), subds = "Edge_Category")
# uwr <- terra::values(uwr)
# uwr <- as.data.frame(uwr)
# uwr$type <- "All edge categories"
# names(uwr)[1] <- "edge_category"
# 
# # Investigating in QGIS reveals this is actually two bands. We just 
# # care about the categorical band, not the alpha band.
# uwr <- uwr[which(uwr$edge_category %in% c(1, 2)), ]
# 
# uwr_p <- rbind(uwr, 
#                data.frame("edge_category" = elk$edge_category,
#                           "type" = "Elk edge categories"))
# uwr_p <- na.omit(uwr_p)
# 
# 
# chisq <- uwr_p |>
#   dplyr::group_by(edge_category, type) |>
#   dplyr::tally() |>
#   tidyr::pivot_wider(names_from = edge_category,
#                      values_from = n) |>
#   tibble::column_to_rownames(var = "type") |>
#   dplyr::mutate_all(~replace(., is.na(.), 0)) |>
#   as.matrix() |>
#   chisq.test()
# 
# corrplot::corrplot(chisq$residuals, is.corr = FALSE)
# 
# 
# ggplot(uwr_p,
#        aes(x = edge_category)) +
#   geom_density(aes(color = type,
#                    fill = type),
#                alpha = 0.1) +
#   scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                     name = "Type",
#                     #labels = c()
#   ) +
#   scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                      name = "Type",
#                      #labels = c()
#   ) +
#   labs(title = "Edge category - available vs elk",
#        subtitle = "All dates",
#        x = "Edge category",
#        y = "Density") +
#   theme_minimal()
# 
# 
# uwr_p <- rbind(uwr, 
#                data.frame("edge_category" = elk[["edge_category"]][lubridate::day(elk$dttm) %in% swp_days],
#                           "type" = "Elk edge categories"))
# uwr_p <- na.omit(uwr_p)
# 
# chisq <- uwr_p |>
#   dplyr::group_by(edge_category, type) |>
#   dplyr::tally() |>
#   tidyr::pivot_wider(names_from = edge_category,
#                      values_from = n) |>
#   tibble::column_to_rownames(var = "type") |>
#   dplyr::mutate_all(~replace(., is.na(.), 0)) |>
#   as.matrix() |>
#   chisq.test()
# 
# corrplot::corrplot(chisq$residuals, is.corr = FALSE)
# 
# ggplot(uwr_p,
#        aes(x = edge_category)) +
#   geom_density(aes(color = type,
#                    fill = type),
#                alpha = 0.1) +
#   scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                     name = "Type",
#                     #labels = c()
#   ) +
#   scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                      name = "Type",
#                      #labels = c()
#   ) +
#   labs(title = "Edge category - available vs elk",
#        subtitle = "During the Severe Winter Period",
#        x = "Edge category",
#        y = "Density") +
#   theme_minimal()
# 
# 
# swp_vs_all <- elk[,c("dttm", "edge_category")]
# swp_vs_all$in_swp <- lubridate::day(swp_vs_all$dttm) %in% swp_days
# swp_vs_all <- sf::st_drop_geometry(swp_vs_all)
# swp_vs_all <- na.omit(swp_vs_all)
# 
# chisq <- swp_vs_all |>
#   dplyr::group_by(edge_category, in_swp) |>
#   dplyr::tally() |>
#   tidyr::pivot_wider(names_from = edge_category,
#                      values_from = n) |>
#   tibble::column_to_rownames(var = "in_swp") |>
#   dplyr::mutate_all(~replace(., is.na(.), 0)) |>
#   as.matrix() |>
#   chisq.test()
# 
# corrplot::corrplot(chisq$residuals, is.corr = FALSE)
# 
# ggplot(swp_vs_all,
#        aes(x = edge_category)) +
#   geom_density(aes(color = in_swp,
#                    fill = in_swp),
#                alpha = 0.1) +
#   scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                     name = "In SWP?",
#                     #labels = c()
#   ) +
#   scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                      name = "In SWP?",
#                      #labels = c()
#   ) +
#   labs(title = "Edge category - Non-SWP vs SWP",
#        x = "Edge category",
#        y = "Density") +
#   theme_minimal()
# 
# 
# ## Edge Distance -----------------------------------------------------------
# 
# 
# uwr <- terra::rast(tar_read(uwr_lidar_gdb), subds = "Edge_Distance_LiDAR")
# uwr <- terra::values(uwr)
# uwr <- as.data.frame(uwr)
# uwr$type <- "All edge distances"
# names(uwr)[1] <- "edge_dist"
# 
# uwr_p <- rbind(uwr, 
#                data.frame("edge_dist" = elk$edge_dist,
#                           "type" = "Elk edge distances"))
# uwr_p <- na.omit(uwr_p)
# 
# 
# ggplot(uwr_p,
#        aes(x = edge_dist)) +
#   geom_density(aes(color = type,
#                    fill = type),
#                alpha = 0.1) +
#   scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                     name = "Type",
#                     #labels = c()
#   ) +
#   scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                      name = "Type",
#                      #labels = c()
#   ) +
#   labs(title = "Edge distance - available vs elk",
#        subtitle = "All dates",
#        x = "Edge distance",
#        y = "Density") +
#   theme_minimal()
# 
# 
# uwr_p <- rbind(uwr, 
#                data.frame("edge_dist" = elk[["edge_dist"]][lubridate::day(elk$dttm) %in% swp_days],
#                           "type" = "Elk edge distances"))
# 
# ggplot(uwr_p,
#        aes(x = edge_dist)) +
#   geom_density(aes(color = type,
#                    fill = type),
#                alpha = 0.1) +
#   scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                     name = "Type",
#                     #labels = c()
#   ) +
#   scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                      name = "Type",
#                      #labels = c()
#   ) +
#   labs(title = "Edge distance - available vs elk",
#        subtitle = "During the Severe Winter Period",
#        x = "Edge distance",
#        y = "Density") +
#   theme_minimal()
# 
# 
# swp_vs_all <- elk[,c("dttm", "edge_dist")]
# swp_vs_all$in_swp <- lubridate::day(swp_vs_all$dttm) %in% swp_days
# 
# ggplot(swp_vs_all,
#        aes(x = edge_dist)) +
#   geom_density(aes(color = in_swp,
#                    fill = in_swp),
#                alpha = 0.1) +
#   scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                     name = "In SWP?",
#                     #labels = c()
#   ) +
#   scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                      name = "In SWP?",
#                      #labels = c()
#   ) +
#   labs(title = "Edge distance - all dates vs severe winter period",
#        x = "Edge distance",
#        y = "Density") +
#   theme_minimal()
# 
# 
# 
# rm(chisq, uwr, uwr_p)
# 
# 
# 
# # CHM ---------------------------------------------------------------------
# 
# # Not using this data, so skip these plots
# 
# ## CHM Data dates ####
# 
# elk |>
#   dplyr::filter(!is.na(crown_height)) |>
#   ggplot(aes(x = dttm, y = TRUE)) +
#   geom_point() +
#   theme_minimal()
# 
# # % data points with BCTS Crown Height data
# round(sum(!is.na(elk$crown_height)) / nrow(elk) * 100)
# 
# unique(sf::st_drop_geometry(elk[["animal_id"]][!is.na(elk$crown_height)])) |>
#   length()
# 
# ## Crown Height ####
# 
# # Available versus what elk chose
# chm <- terra::rast("GIS/LiDAR products/crown_height.tif")
# 
# # Extract DEM values from the random pts coordinates
# random_pts <- cbind(random_pts,
#                     terra::extract(chm, random_pts, ID = FALSE))
# 
# # Create a df of data to plot - our elk data
# # merged with the random sampling data
# p <- rbind(data.frame("val" = random_pts$crown_height,
#                       "type" = "Random crown height",
#                       "dttm" = random_pts$dttm,
#                       "season" = random_pts$season,
#                       "pool" = random_pts$pool),
#            data.frame("val" = elk$crown_height,
#                       "type" = "Elk crown height",
#                       "dttm" = elk$dttm,
#                       "season" = elk$season,
#                       "pool" = "real data"))
# 
# p <- p[!is.na(p$val), ] # cut out NA areas - we don't have full LiDAR coverage
# 
# # DENSITY PLOT - Random vs Selected
# p|>
#   dplyr::filter(pool %in% c("real data", "full study area")) |>
#   ggplot(aes(x = val)) +
#   geom_density(aes(color = type,
#                    fill = type),
#                alpha = 0.1) +
#   scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                     name = "Type",
#                     #labels = c()
#   ) +
#   scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                      name = "Type",
#                      #labels = c()
#   ) +
#   labs(title = "Slope grade - available vs elk",
#        subtitle = "All dates",
#        x = "Slope (%)",
#        y = "Density",
#        caption = paste0("N elk data points = ", nrow(p[p$type == "Elk crown height", ]), "; N random data points = ", nrow(p[p$type == "Random crown height", ]))) +
#   theme_minimal()
# 
# # Seasonal differences
# 
# # Set factor for plotting
# p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))
# 
# ## BOXPLOT
# p |>
#   dplyr::filter(!is.na(season)) |>
#   dplyr::filter(season %in% c("Winter", "Spring", "Summer")) |>
#   dplyr::group_by(season, type) |>
#   dplyr::mutate(N = dplyr::n()) |> # calculate N points per group
#   dplyr::mutate(N = paste0("N = ", as.character(N))) |> # turn it into a plot label
#   dplyr::mutate(N = ifelse(val == max(val, na.rm = T),
#                            N,
#                            NA)) |> # now get rid of all the duplicate labels so it doesn't plot literally thousands of them. Only plot 1 label at the max value of "m" per group.
#   ggplot(aes(x = type,
#              y = val)) +
#   geom_jitter(aes(color = season),
#               alpha = 0.1) +
#   geom_boxplot(fill = NA) +
#   scale_color_manual("Season", values = okabe[1:4]) +
#   geom_signif(comparisons = list(c("Elk crown height", "Random crown height")),
#               test = "wilcox.test",
#               map_signif_level = TRUE,
#               y_position = 80) +
#   geom_text(aes(label = N),
#             nudge_y = 10) +
#   facet_wrap(~ season) +
#   labs(x = "",
#        y = "Crown Height (m)",
#        caption = "Differing sample sizes within seasons are due to uneven BCTS LiDAR coverage between where elk vs. random points may have been.") +
#   theme_minimal()
# 
# 
# ## DENSITY PLOT
# p |>
#   dplyr::filter(!is.na(season)) |>
#   dplyr::filter(pool %in% c("real data", "full study area")) |>
#   dplyr::mutate(season = dplyr::if_else(type == "Elk crown height",
#                                         season,
#                                         "Random points")) |>
#   dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
#   ggplot(aes(x = val,
#              color = season,
#              fill = season)) +
#   geom_density(alpha = 0.2) +
#   scale_color_manual("Season",
#                      values = okabe[1:4]) +
#   scale_fill_manual("Season",
#                     values = okabe[1:4]) +
#   labs(x = "Crown Height (m)",
#        y = "Density") +
#   theme_minimal()
# 
# 
# p |>
#   dplyr::filter(!is.na(season)) |>
#   dplyr::filter(type == "Elk crown height") |>
#   ggplot(aes(x = val,
#              color = season,
#              fill = season)) +
#   geom_density(alpha = 0.2) +
#   scale_color_manual("Season",
#                      values = okabe[1:4]) +
#   scale_fill_manual("Season",
#                     values = okabe[1:4]) +
#   labs(x = "Crown Height (m)",
#        y = "Density") +
#   theme_minimal()
# 
# 
# ## DENSITY PLOT SPLIT BY SEASON
# p |>
#   dplyr::filter(!is.na(season)) |>
#   dplyr::filter(pool %in% c("real data", "seasonal MCPs")) |>
#   dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
#   ggplot(aes(x = val,
#              color = season,
#              fill = season,
#              linetype = pool)) +
#   geom_density(alpha = 0.2) +
#   scale_linetype("Source", labels = c("Real data", "Random data")) +
#   scale_color_manual("Season",
#                      values = okabe[1:4]) +
#   scale_fill_manual("Season",
#                     values = okabe[1:4]) +
#   facet_wrap(~ season) +
#   labs(x = "Crown Height (m)",
#        y = "Density",
#        caption = "100k random data points for each season are sampled from that season's MCP. \nSeasonal MCPs are derived by merging all the MCPs from each elk-season into a single polygon.") +
#   theme_minimal()
# 
# 
# # SWP plots
# p$yday <- lubridate::yday(p$dttm)
# p$year <- lubridate::year(p$dttm)
# 
# 
# ## BOXPLOT - Yearly variation WITHOUT random
# p |>
#   dplyr::filter(pool == "real") |>
#   dplyr::filter(yday %in% swp_days) |>
#   dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
#   dplyr::mutate(year = paste0(year, "-", year+1)) |>
#   #units::drop_units() |>
#   ggplot(aes(x = as.factor(year),
#              y = val,
#              color = as.factor(year))) +
#   geom_jitter(alpha = 0.1) +
#   geom_boxplot(fill = NA) +
#   scale_color_manual(values = okabe[1:5],
#                      name = "Type") +
#   labs(title = "Crown Height - SWP across years",
#        subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
#        x = "Year",
#        y = "Crown Height (m)",
#        caption = "2021-2022 was the 'Severe' year.") +
#   geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
#                                  c("2021-2022", "2020-2021"),
#                                  c("2021-2022", "2022-2023"),
#                                  c("2022-2023", "2023-2024")),
#               map_signif_level = TRUE,
#               test = "wilcox.test",
#               color = "black") +
#   geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
#                                  c("2021-2022", "2023-2024")),
#               map_signif_level = TRUE,
#               test = "wilcox.test",
#               y_position = 68,
#               color = "black") +
#   geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
#               map_signif_level = TRUE,
#               test = "wilcox.test",
#               y_position = 75,
#               color = "black") +
#   geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
#               map_signif_level = TRUE,
#               test = "wilcox.test",
#               y_position = 80,
#               color = "black") +
#   theme_minimal()
# 
# 
# ## BOXPLOT - Yearly variation WITH RANDOM
# p |>
#   dplyr::filter(swp == TRUE) |>
#   dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
#   dplyr::mutate(year = paste0(year, "-", year+1)) |>
#   dplyr::mutate(year = dplyr::if_else(pool == "random", "Random", year)) |>
#   #units::drop_units() |>
#   ggplot(aes(x = as.factor(year),
#              y = val)) +
#   geom_jitter(aes(color = as.factor(year)),
#               alpha = 0.1) +
#   geom_boxplot(fill = NA) +
#   scale_color_manual(values = okabe[1:6],
#                      name = "Type") +
#   labs(title = "Crown Height - SWP across years",
#        subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
#        x = "Year",
#        y = "Crown Height (m)",
#        caption = "2021-2022 was the 'Severe' year.
#        50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
#        Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
#   geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
#                                  c("2021-2022", "2020-2021"),
#                                  c("2021-2022", "2022-2023"),
#                                  c("2022-2023", "2023-2024")),
#               map_signif_level = TRUE,
#               test = "wilcox.test",
#               y_position = 68,
#               color = "black") +
#   geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
#                                  c("2021-2022", "2023-2024")),
#               map_signif_level = TRUE,
#               test = "wilcox.test",
#               y_position = 75,
#               color = "black") +
#   geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
#               map_signif_level = TRUE,
#               test = "wilcox.test",
#               y_position = 80,
#               color = "black") +
#   geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
#               map_signif_level = TRUE,
#               test = "wilcox.test",
#               y_position = 85,
#               color = "black") +
#   theme_minimal()
# 
# ## DENSITY PLOT
# p |>
#   dplyr::filter(pool == "real") |>
#   dplyr::filter(yday %in% swp_days) |>
#   dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
#   dplyr::mutate(year = paste0(year, "-", year+1)) |>
#   #units::drop_units() |>
#   ggplot(aes(x = val,
#              color = as.factor(year))) +
#   geom_density(aes(color = year,
#                    fill = year),
#                alpha = 0.1) +
#   scale_fill_manual(values = okabe[1:5],
#                     name = "Year",
#                     #labels = c()
#   ) +
#   scale_color_manual(values = okabe[1:5],
#                      name = "Year",
#                      #labels = c()
#   ) +
#   labs(title = "Crown Height - SWP across years",
#        subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
#        x = "Crown Height (m)",
#        y = "Density",
#        caption = "2021-2022 was the 'Severe' year.") +
#   theme_minimal()
# 
# 
# ## SWP ACTUAL VS RANDOM
# # Random SWP points are sampled from the the pooled Winter MCPs
# # of elk individuals that have experienced a severe winter.
# p |>
#   dplyr::filter(swp == TRUE & pool == "random") |>
#   dplyr::select(val, yday, year, type, pool) |>
#   # Duplicate random dataset for each year
#   dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
#   tidyr::unnest(cols = c(year)) |>
#   # Bind in the real data now
#   dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
#   dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
#   dplyr::mutate(year = paste0(year, "-", year+1)) |>
#   dplyr::filter(yday %in% swp_days) |>
#   dplyr::filter(year %in% c("2019-2020", "2020-2021",
#                             "2021-2022", "2022-2023", "2023-2024")) |>
#   #units::drop_units() |>
#   ggplot() +
#   geom_density(aes(x = val,
#                    color = year,
#                    fill = year,
#                    linetype = pool),
#                alpha = 0.1) +
#   scale_linetype("Source", labels = c("Real data", "Random data")) +
#   scale_fill_manual(values = okabe[1:7],
#                     name = "Year",
#                     #labels = c()
#   ) +
#   scale_color_manual(values = okabe[1:7],
#                      name = "Year",
#                      #labels = c()
#   ) +
#   facet_wrap(~ year) +
#   labs(title = "Crown Height - SWP across years",
#        subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
#        x = "Crown Height (m)",
#        y = "Density",
#        caption = "2021-2022 was the 'Severe' year.
#        50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
#        Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
#   theme_minimal()
# 

# VRI ---------------------------------------------------------------------

## Stand Age ####

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$PROJ_AGE_1,
                      "type" = "Random stand age",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP),
           data.frame("val" = elk$proj_age_1,
                      "type" = "Elk stand age",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP))

# DENSITY PLOT - Random vs Selected
p |>
  ggplot(aes(x = val)) +
  geom_density(aes(color = type,
                   fill = type),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Type",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Type",
                     #labels = c()
  ) +
  labs(title = "Stand Age - available vs elk",
       subtitle = "All dates",
       x = "Stand Age",
       y = "Density",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(season %in% c("Winter", "Spring", "Summer")) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              pch = ".",
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk stand age", "Random stand age")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 810) +
  geom_text(aes(label = N),
            nudge_y = 10) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Stand Age") +
  theme_minimal()



## DENSITY PLOT
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = dplyr::if_else(pool == "real",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Stand Age",
       y = "Density") +
  theme_minimal()


p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(pool == "real") |>
  units::drop_units() |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Stand Age",
       y = "Density") +
  theme_minimal()


## DENSITY PLOT SPLIT BY SEASON
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season,
             linetype = type)) +
  geom_density(alpha = 0.2) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  facet_wrap(~ season) +
  labs(x = "Stand Age",
       y = "Density",
       caption = "100k random data points for each season are sampled from that season's MCP. \nSeasonal MCPs are derived by merging all the MCPs from each elk-season into a single polygon.") +
  theme_minimal()



# SWP plots

## BOXPLOT - Yearly variation WITHOUT random
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Stand Age - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Stand Age",
       caption = "2021-2022 was the 'Severe' year.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 700,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 750,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 790,
              color = "black") +
  theme_minimal()


## BOXPLOT - Yearly variation WITH RANDOM
p |>
  dplyr::filter(swp == TRUE) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(year = dplyr::if_else(pool == "random", "Random", year)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:6],
                     name = "Type") +
  labs(title = "Stand Age - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Stand Age",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 700,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 750,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 790,
              color = "black") +
  theme_minimal()


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = val,
             color = as.factor(year))) +
  geom_density(aes(color = year,
                   fill = year),
               alpha = 0.1) +
  scale_fill_manual(values = okabe[1:5],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:5],
                     name = "Year",
                     #labels = c()
  ) +
  labs(title = "Stand Age - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Stand Age",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()



## SWP ACTUAL VS RANDOM
# Random SWP points are sampled from the the pooled Winter MCPs
# of elk individuals that have experienced a severe winter.
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  #units::drop_units() |>
  ggplot() +
  geom_density(aes(x = val,
                   color = year,
                   fill = year,
                   linetype = type),
               alpha = 0.1) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_fill_manual(values = okabe[1:7],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:7],
                     name = "Year",
                     #labels = c()
  ) +
  facet_wrap(~ year) +
  labs(title = "Stand Age - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Stand Age",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()




## Stand Height ####


# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$PROJ_HEIGHT_1,
                      "type" = "Random stand height",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP),
           data.frame("val" = elk$proj_height_1,
                      "type" = "Elk stand height",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP))

# DENSITY PLOT - Random vs Selected
p |>
  ggplot(aes(x = val)) +
  geom_density(aes(color = type,
                   fill = type),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Type",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Type",
                     #labels = c()
  ) +
  labs(title = "Stand Height - available vs elk",
       subtitle = "All dates",
       x = "Stand Height (m)",
       y = "Density",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(pool %in% c("real data", "seasonal MCPs")) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(season %in% c("Winter", "Spring", "Summer")) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              pch = ".",
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk stand height", "Random stand height")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 100) +
  geom_text(aes(label = N),
            nudge_y = 10) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Stand Height (m)") +
  theme_minimal()



## DENSITY PLOT
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = dplyr::if_else(pool == "real",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Stand Height (m)",
       y = "Density") +
  theme_minimal()


p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(pool %in% c("real")) |>
  units::drop_units() |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Stand Height (m)",
       y = "Density") +
  theme_minimal()


## DENSITY PLOT SPLIT BY SEASON
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season,
             linetype = type)) +
  geom_density(alpha = 0.2) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  facet_wrap(~ season) +
  labs(x = "Stand Height (m)",
       y = "Density",
       caption = "100k random data points for each season are sampled from that season's MCP. \nSeasonal MCPs are derived by merging all the MCPs from each elk-season into a single polygon.") +
  theme_minimal()



# SWP plots

## BOXPLOT - Yearly variation WITHOUT random
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Stand Height - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Stand Height (m)",
       caption = "2021-2022 was the 'Severe' year.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 85,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 95,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 105,
              color = "black") +
  theme_minimal()


## BOXPLOT - Yearly variation WITH RANDOM
p |>
  dplyr::filter(swp == TRUE) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(year = dplyr::if_else(pool == "random", "Random", year)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:6],
                     name = "Type") +
  labs(title = "Stand Height - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Stand Height (m)",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 75,
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 85,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 95,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 105,
              color = "black") +
  theme_minimal()


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = val,
             color = as.factor(year))) +
  geom_density(aes(color = year,
                   fill = year),
               alpha = 0.1) +
  scale_fill_manual(values = okabe[1:5],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:5],
                     name = "Year",
                     #labels = c()
  ) +
  labs(title = "Stand Height - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Stand Height (m)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()


## SWP ACTUAL VS RANDOM
# Random SWP points are sampled from the the pooled Winter MCPs
# of elk individuals that have experienced a severe winter.
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  #units::drop_units() |>
  ggplot() +
  geom_density(aes(x = val,
                   color = year,
                   fill = year,
                   linetype = type),
               alpha = 0.1) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_fill_manual(values = okabe[1:7],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:7],
                     name = "Year",
                     #labels = c()
  ) +
  facet_wrap(~ year) +
  labs(title = "Stand Height - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Stand Height (m)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()




## Stem Density ####

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$VRI_LIVE_STEMS_PER_HA,
                      "type" = "Random stem density",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP),
           data.frame("val" = elk$vri_live_stems_per_ha,
                      "type" = "Elk stem density",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP))

# DENSITY PLOT - Random vs Selected
p |>
  ggplot(aes(x = val)) +
  geom_density(aes(color = type,
                   fill = type),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Type",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Type",
                     #labels = c()
  ) +
  labs(title = "Live Stem Density - available vs elk",
       subtitle = "All dates",
       x = "Live Stem Density (stems/ha)",
       y = "Density",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(season %in% c("Winter", "Spring", "Summer")) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              pch = ".",
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk stem density", "Random stem density")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 12000) +
  geom_text(aes(label = N),
            nudge_y = 10) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Live Stem Density (stems/ha)") +
  theme_minimal()



## DENSITY PLOT
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = dplyr::if_else(pool == "real",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Live Stem Density (stems/ha)",
       y = "Density") +
  theme_minimal()

# Same but zoomed
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = dplyr::if_else(pool == "real",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  units::drop_units() |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  scale_x_continuous(limits = c(0, 5000)) +
  labs(x = "Live Stem Density (stems/ha)",
       y = "Density") +
  theme_minimal()


p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(pool == "real") |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  units::drop_units() |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Live Stem Density (stems/ha)",
       y = "Density") +
  theme_minimal()


## DENSITY PLOT SPLIT BY SEASON
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season,
             linetype = type)) +
  geom_density(alpha = 0.2) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  facet_wrap(~ season) +
  labs(x = "Live Stem Density (stems/ha)",
       y = "Density",
       caption = "100k random data points for each season are sampled from that season's MCP. \nSeasonal MCPs are derived by merging all the MCPs from each elk-season into a single polygon.") +
  theme_minimal()


# SWP plots

## BOXPLOT - Yearly variation WITHOUT random
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)), 
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Live Stem Density - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Live Stem Density (stems/ha)",
       caption = "2021-2022 was the 'Severe' year.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 11500,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 13000,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 14000,
              color = "black") +
  theme_minimal()


## BOXPLOT - Yearly variation WITH RANDOM
p |>
  dplyr::filter(swp == TRUE) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(year = dplyr::if_else(pool == "random", "Random", year)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:6],
                     name = "Type") +
  labs(title = "Live Stem Density - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Live Stem Density (stems/ha)",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 11500,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 13000,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 14000,
              color = "black") +
  theme_minimal()


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = val,
             color = as.factor(year))) +
  geom_density(aes(color = year,
                   fill = year),
               alpha = 0.1) +
  scale_fill_manual(values = okabe[1:5],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:5],
                     name = "Year",
                     #labels = c()
  ) +
  labs(title = "Live Stem Density - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Live Stem Density (stems/ha)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()


## SWP ACTUAL VS RANDOM
# Random SWP points are sampled from the the pooled Winter MCPs
# of elk individuals that have experienced a severe winter.
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  #units::drop_units() |>
  ggplot() +
  geom_density(aes(x = val,
                   color = year,
                   fill = year,
                   linetype = type),
               alpha = 0.1) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_fill_manual(values = okabe[1:7],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:7],
                     name = "Year",
                     #labels = c()
  ) +
  facet_wrap(~ year) +
  labs(title = "Live Stem Density - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Live Stem Density (stems/ha)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()



## Crown Closure ####


# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$CROWN_CLOSURE,
                      "type" = "Random cc",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP),
           data.frame("val" = elk$crown_closure,
                      "type" = "Elk cc",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP))

# DENSITY PLOT - Random vs Selected
p |>
  ggplot(aes(x = val)) +
  geom_density(aes(color = type,
                   fill = type),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Type",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Type",
                     #labels = c()
  ) +
  labs(title = "Crown Closure - available vs elk",
       subtitle = "All dates",
       x = "Crown Closure (%)",
       y = "Density",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(season %in% c("Winter", "Spring", "Summer")) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              pch = ".",
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk cc", "Random cc")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 120) +
  geom_text(aes(label = N),
            nudge_y = 10) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Crown Closure (%)",
       caption = "Differing sample sizes between groups are caused in cases where a GPS point \nmay have landed on an empty space where a deleted sliver polygon may have been.") +
  theme_minimal()



## DENSITY PLOT
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = dplyr::if_else(pool == "real",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Crown Closure (%)",
       y = "Density") +
  theme_minimal()


p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(type == "Elk cc") |>
  units::drop_units() |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Crown Closure (%)",
       y = "Density") +
  theme_minimal()


## DENSITY PLOT SPLIT BY SEASON
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season,
             linetype = type)) +
  geom_density(alpha = 0.2) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  facet_wrap(~ season) +
  labs(x = "Crown Closure (%)",
       y = "Density",
       caption = "100k random data points for each season are sampled from that season's MCP. \nSeasonal MCPs are derived by merging all the MCPs from each elk-season into a single polygon.") +
  theme_minimal()



# SWP plots

## BOXPLOT - Yearly variation WITHOUT random
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)), 
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Crown Closure - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Crown Closure (%)",
       caption = "2021-2022 was the 'Severe' year.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 105,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 120,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 130,
              color = "black") +
  theme_minimal()


## BOXPLOT - Yearly variation WITH RANDOM
p |>
  dplyr::filter(swp == TRUE) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(year = dplyr::if_else(pool == "random", "Random", year)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:6],
                     name = "Type") +
  labs(title = "Crown Closure - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Crown Closure (%)",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 105,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 120,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 130,
              color = "black") +
  theme_minimal()

## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = val,
             color = as.factor(year))) +
  geom_density(aes(color = year,
                   fill = year),
               alpha = 0.1) +
  scale_fill_manual(values = okabe[1:5],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:5],
                     name = "Year",
                     #labels = c()
  ) +
  labs(title = "Crown Closure - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Crown Closure (%)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()


## SWP ACTUAL VS RANDOM
# Random SWP points are sampled from the the pooled Winter MCPs
# of elk individuals that have experienced a severe winter.
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  #units::drop_units() |>
  ggplot() +
  geom_density(aes(x = val,
                   color = year,
                   fill = year,
                   linetype = type),
               alpha = 0.1) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_fill_manual(values = okabe[1:7],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:7],
                     name = "Year",
                     #labels = c()
  ) +
  facet_wrap(~ year) +
  labs(title = "Crown Closure - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Crown Closure (%)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()



## Shrub Height ####

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$SHRUB_HEIGHT,
                      "type" = "Random shrub height",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP),
           data.frame("val" = elk$shrub_height,
                      "type" = "Elk shrub height",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP))

# DENSITY PLOT - Random vs Selected
p |>
  ggplot(aes(x = val)) +
  geom_density(aes(color = type,
                   fill = type),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Type",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Type",
                     #labels = c()
  ) +
  labs(title = "Shrub Height - available vs elk",
       subtitle = "All dates",
       x = "Shrub Height (m)",
       y = "Density",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(season %in% c("Winter", "Spring", "Summer")) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              pch = ".",
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk shrub height", "Random shrub height")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 12) +
  geom_text(aes(label = N),
            nudge_y = 1) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Shrub Height (m)",
       caption = "Differing sample sizes between groups are caused in cases where a GPS point \nmay have landed on an empty space where a deleted sliver polygon may have been.") +
  theme_minimal()



## DENSITY PLOT
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = dplyr::if_else(type == "Elk shrub height",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Shrub Height (m)",
       y = "Density") +
  theme_minimal()


p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(type == "Elk shrub height") |>
  units::drop_units() |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  scale_x_continuous(limits = c(0, 3)) +
  labs(x = "Shrub Height (m)",
       y = "Density") +
  theme_minimal()


## DENSITY PLOT SPLIT BY SEASON
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season,
             linetype = type)) +
  geom_density(alpha = 0.2) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  facet_wrap(~ season) +
  labs(x = "Shrub Height (m)",
       y = "Density",
       caption = "100k random data points for each season are sampled from that season's MCP. \nSeasonal MCPs are derived by merging all the MCPs from each elk-season into a single polygon.") +
  theme_minimal()


# SWP plots

## BOXPLOT - Yearly variation WITHOUT random
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Shrub Height - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Shrub Height (m)",
       caption = "2021-2022 was the 'Severe' year.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 6,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 7,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 8,
              color = "black") +
  theme_minimal()


## BOXPLOT - Yearly variation WITH RANDOM
p |>
  dplyr::filter(swp == TRUE) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(year = dplyr::if_else(pool == "random", "Random", year)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:6],
                     name = "Type") +
  labs(title = "Shrub Height - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Shrub Height (m)",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 6,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 7,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 8,
              color = "black") +
  theme_minimal()


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = val,
             color = as.factor(year))) +
  geom_density(aes(color = year,
                   fill = year),
               alpha = 0.1) +
  scale_fill_manual(values = okabe[1:5],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:5],
                     name = "Year",
                     #labels = c()
  ) +
  scale_x_continuous(limits = c(0,4)) +
  labs(title = "Shrub Height - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Shrub Height (m)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()


## SWP ACTUAL VS RANDOM
# Random SWP points are sampled from the the pooled Winter MCPs
# of elk individuals that have experienced a severe winter.
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  #units::drop_units() |>
  ggplot() +
  geom_density(aes(x = val,
                   color = year,
                   fill = year,
                   linetype = type),
               alpha = 0.1) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_fill_manual(values = okabe[1:7],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:7],
                     name = "Year",
                     #labels = c()
  ) +
  scale_x_continuous(limits = c(0,4)) +
  facet_wrap(~ year) +
  labs(title = "Shrub Height - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Shrub Height (m)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()




## Shrub Crown Closure ####

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$SHRUB_CROWN_CLOSURE,
                      "type" = "Random shrub cc",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP),
           data.frame("val" = elk$shrub_crown_closure,
                      "type" = "Elk shrub cc",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP))

# DENSITY PLOT - Random vs Selected
p |>
  ggplot(aes(x = val)) +
  geom_density(aes(color = type,
                   fill = type),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Type",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Type",
                     #labels = c()
  ) +
  labs(title = "Shrub Crown Closure - available vs elk",
       subtitle = "All dates",
       x = "Shrub Crown Closure (%)",
       y = "Density",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(season %in% c("Winter", "Spring", "Summer")) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              pch = ".",
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk shrub cc", "Random shrub cc")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 120) +
  geom_text(aes(label = N),
            nudge_y = 10) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Shrub Crown Closure (%)",
       caption = "Differing sample sizes between groups are caused in cases where a GPS point \nmay have landed on an empty space where a deleted sliver polygon may have been.") +
  theme_minimal()



## DENSITY PLOT
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = dplyr::if_else(type == "Elk shrub cc",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Shrub Crown Closure (%)",
       y = "Density") +
  theme_minimal()


p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(type == "Elk shrub cc") |>
  units::drop_units() |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Shrub Crown Closure (%)",
       y = "Density") +
  theme_minimal()

## DENSITY PLOT SPLIT BY SEASON
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season,
             linetype = type)) +
  geom_density(alpha = 0.2) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  facet_wrap(~ season) +
  labs(x = "Shrub Crown Closure (%)",
       y = "Density",
       caption = "100k random data points for each season are sampled from that season's MCP. \nSeasonal MCPs are derived by merging all the MCPs from each elk-season into a single polygon.") +
  theme_minimal()


# SWP plots

## BOXPLOT - Yearly variation WITHOUT random
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)), 
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Shrub Crown Closure - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Shrub Crown Closure (%)",
       caption = "2021-2022 was the 'Severe' year.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 120,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 130,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 140,
              color = "black") +
  theme_minimal()


## BOXPLOT - Yearly variation WITH RANDOM
p |>
  dplyr::filter(swp == TRUE) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(year = dplyr::if_else(pool == "random", "Random", year)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:6],
                     name = "Type") +
  labs(title = "Shrub Crown Closure - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Shrub Crown Closure (%)",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 120,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 130,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 140,
              color = "black") +
  theme_minimal()


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = val,
             color = as.factor(year))) +
  geom_density(aes(color = year,
                   fill = year),
               alpha = 0.1) +
  scale_fill_manual(values = okabe[1:5],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:5],
                     name = "Year",
                     #labels = c()
  ) +
  labs(title = "Shrub Crown Closure - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Shrub Crown Closure (%)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()


## SWP ACTUAL VS RANDOM
# Random SWP points are sampled from the the pooled Winter MCPs
# of elk individuals that have experienced a severe winter.
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  #units::drop_units() |>
  ggplot() +
  geom_density(aes(x = val,
                   color = year,
                   fill = year,
                   linetype = type),
               alpha = 0.1) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_fill_manual(values = okabe[1:7],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:7],
                     name = "Year",
                     #labels = c()
  ) +
  facet_wrap(~ year) +
  labs(title = "Shrub Crown Closure - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Shrub Crown Closure (%)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()


## Land Cover Class ####


# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$BCLCS_LEVEL_4,
                      "type" = "Random Land Cover Class",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP),
           data.frame("val" = elk$bclcs_level_4,
                      "type" = "Elk Land Cover Class",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP))

# Remove NA
p <- p[!is.na(p$val), ]

# STACKED BAR - Random vs Selected
p |> 
  ggplot(aes(val)) + 
  geom_bar(aes(fill = type),
           alpha = 0.7) +
  scale_fill_manual(values = c("#E69F00", "#009E73"),
                    name = "Type",
                    #labels = c()
  ) +
  labs(title = "Land Cover Class - available vs elk",
       subtitle = "All dates",
       x = "Land Cover Class",
       y = "Proportion",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# PROPORTIONAL BAR - Random vs Selected
p |> 
  ggplot(aes(val)) + 
  geom_bar(aes(fill = type),
           position = "fill",
           alpha = 0.7) +
  scale_fill_manual(values = c("#E69F00", "#009E73"),
                    name = "Type",
                    #labels = c()
  ) +
  labs(title = "Land Cover Class - available vs elk",
       subtitle = "All dates",
       x = "Land Cover Class",
       y = "Proportion",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

p <- p |> dplyr::mutate(val = dplyr::if_else(val %in% c("HE", "HG", "SL", "TB", "TC", "TM"),
                                             val,
                                             "Other")) |>
  dplyr::mutate(val = factor(val, c("TC", "TM", "TB", "SL", "HE", "HG", "Other")))

# SEASONAL MOSAIC
p |> 
  dplyr::filter(pool == "real") |> 
  dplyr::select(val, type) |>
  dplyr::filter(!is.na(type)) |>
  ggplot() +
  geom_mosaic(aes(x = product(val),
                  fill = type)) +
  scale_fill_manual("Season",
                    values = okabe[1:3]) +
  #scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7")) +
  #khroma::scale_fill_okabeito(black_position = "last") +
  labs(x = "Land Cover Class",
       y = "Season") +
  coord_flip() +
  theme_mosaic()


# PROPORTIONAL BAR - Seasonal Random vs Selected
p |> 
  dplyr::filter(!is.na(season)) |> 
  ggplot(aes(val)) + 
  geom_bar(aes(fill = type),
           position = "fill",
           alpha = 0.7) +
  facet_wrap(~ season, nrow = 3) +
  labs(title = "Land Cover Class - available vs elk",
       subtitle = "All dates",
       x = "Land Cover Class",
       y = "Proportion",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Spring
s1 <- p |> 
  dplyr::filter(season == "Spring") |> 
  ggplot(aes(val)) + 
  geom_bar(aes(alpha = type),
           position = "fill",
           fill = okabe[1]) +
  scale_alpha_manual("Type", values = c(0.8, 0.3)) +
  facet_wrap(~ season, nrow = 3) +
  labs(y = "Proportion") +
  theme_minimal()

# Summer
s2 <- p |> 
  dplyr::filter(season == "Summer") |> 
  ggplot(aes(val)) + 
  geom_bar(aes(alpha = type),
           position = "fill",
           fill = okabe[2]) +
  scale_alpha_manual("Type", values = c(0.8, 0.3)) +
  facet_wrap(~ season, nrow = 3) +
  labs(y = "Proportion") +
  theme_minimal()

# Winter
s3 <- p |> 
  dplyr::filter(season == "Winter") |> 
  ggplot(aes(val)) + 
  geom_bar(aes(alpha = type),
           position = "fill",
           fill = okabe[3]) +
  scale_alpha_manual("Type", values = c(0.8, 0.3)) +
  facet_wrap(~ season, nrow = 3) +
  labs(x = "Land Cover Class",
       y = "Proportion") +
  theme_minimal()


ggpubr::ggarrange(s1, s2, s3, ncol = 1)


# SWP plots

# MOSAIC - Yearly SWP variation
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::select(val, year) |>
  ggplot() +
  geom_mosaic(aes(x = product(val),
                  fill = year)) +
  scale_fill_manual("Year",
                    values = okabe[1:5]) +
  labs(x = "Land Cover Class",
       y = "Year",
       caption = "2021-2022 was the 'Severe' year.") +
  coord_flip() +
  theme_mosaic()

# PROPORTIONAL AREA - Yearly SWP
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Filter down to same proportion as real data
  dplyr::slice_sample(n = round(nrow(p[which(p$swp == TRUE & p$pool == "real"), ])/5)) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  dplyr::select(val, type, year) |>
  ggplot(aes(val)) + 
  geom_bar(aes(fill = year,
               alpha = type),
           position = "fill") +
  scale_fill_manual("Year", 
                    values = okabe[1:5]) +
  scale_alpha_manual("Type", values = c(0.8, 0.3)) +
  facet_wrap(~ year, ncol = 1) +
  labs(title = "Land Cover Class - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Land Cover Class",
       y = "Proportion",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()


## Primary Species Composition ####

# Note VRI fixes done at the start of this section + start of document
# prior to running this code

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$SPECIES_CD_1,
                      "type" = "Random Primary Spp",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP),
           data.frame("val" = elk$species_cd_1,
                      "type" = "Elk Primary Spp",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP))

# Remove NA
p <- p[!is.na(p$val), ]

# STACKED BAR - Random vs Selected
p |> 
  ggplot(aes(val)) + 
  geom_bar(aes(fill = type),
           alpha = 0.7) +
  scale_fill_manual(values = c("#E69F00", "#009E73"),
                    name = "Type",
                    #labels = c()
  ) +
  labs(title = "Primary Species - available vs elk",
       subtitle = "All dates",
       x = "Primary Species",
       y = "Proportion",
       caption = paste0("N data points real data = ", 
                        nrow(p[p$pool == "real data",]),
                        "\nN data points random data = ", 
                        nrow(p[p$pool != "real data",]))
       ) +
  theme_minimal()

# PROPORTIONAL BAR - Random vs Selected
p |> 
  ggplot(aes(val)) + 
  geom_bar(aes(fill = type),
           position = "fill",
           alpha = 0.7) +
  scale_fill_manual(values = c("#E69F00", "#009E73"),
                    name = "Type",
                    #labels = c()
  ) +
  labs(title = "Primary Species - available vs elk",
       subtitle = "All dates",
       x = "Primary Species",
       y = "Proportion",
       caption = paste0("N data points real data = ", 
                        nrow(p[p$pool == "real data",]),
                        "\nN data points random data = ", 
                        nrow(p[p$pool != "real data",]))
       ) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

p <- p |> dplyr::mutate(val = dplyr::if_else(val %in% c("ACT", "BA", "CW", "DR",
                                                        "FDC", "HM", "HW", "SS", "YC"),
                                             val,
                                             "Other")) |>
  dplyr::mutate(val = factor(val, c("HW", "FDC", "BA", "YC",
                                    "HM", "CW", "DR", "SS", "ACT",
                                    "Other")))

# SEASONAL MOSAIC
p |> 
  dplyr::filter(pool %in% c("real")) |> 
  dplyr::mutate(type = dplyr::if_else(pool == "real",
                                      season,
                                      type)) |>
  dplyr::select(val, type) |>
  dplyr::filter(!is.na(type)) |>
  ggplot() +
  geom_mosaic(aes(x = product(val),
                  fill = type)) +
  scale_fill_manual("Season",
                    values = okabe[1:3]) +
  #scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7")) +
  #khroma::scale_fill_okabeito(black_position = "last") +
  labs(x = "Primary Species",
       y = "Season") +
  coord_flip() +
  theme_mosaic()


# PROPORTIONAL BAR - Seasonal Random vs Selected
p |> 
  dplyr::filter(!is.na(season)) |> 
  ggplot(aes(val)) + 
  geom_bar(aes(fill = type),
           position = "fill",
           alpha = 0.7) +
  facet_wrap(~ season, nrow = 3) +
  labs(title = "Primary Species - available vs elk",
       subtitle = "All dates",
       x = "Primary Speices",
       y = "Proportion",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Spring
s1 <- p |> 
  dplyr::filter(season == "Spring") |> 
  ggplot(aes(val)) + 
  geom_bar(aes(alpha = type),
           position = "fill",
           fill = okabe[1]) +
  scale_alpha_manual("Type", values = c(0.8, 0.3)) +
  facet_wrap(~ season, nrow = 3) +
  labs(y = "Proportion") +
  theme_minimal()

# Summer
s2 <- p |> 
  dplyr::filter(season == "Summer") |> 
  ggplot(aes(val)) + 
  geom_bar(aes(alpha = type),
           position = "fill",
           fill = okabe[2]) +
  scale_alpha_manual("Type", values = c(0.8, 0.3)) +
  facet_wrap(~ season, nrow = 3) +
  labs(y = "Proportion") +
  theme_minimal()

# Winter
s3 <- p |> 
  dplyr::filter(season == "Winter") |> 
  ggplot(aes(val)) + 
  geom_bar(aes(alpha = type),
           position = "fill",
           fill = okabe[3]) +
  scale_alpha_manual("Type", values = c(0.8, 0.3)) +
  facet_wrap(~ season, nrow = 3) +
  labs(x = "Primary Species",
       y = "Proportion") +
  theme_minimal()


ggpubr::ggarrange(s1, s2, s3, ncol = 1)


# SWP plots

# MOSAIC - Yearly SWP variation
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::select(val, year) |>
  ggplot() +
  geom_mosaic(aes(x = product(val),
                  fill = year)) +
  scale_fill_manual("Year",
                    values = okabe[1:5]) +
  labs(x = "Primary Species",
       y = "Year",
       caption = "2021-2022 was the 'Severe' year.") +
  coord_flip() +
  theme_mosaic()

# PROPORTIONAL AREA - Yearly SWP
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Filter down to same proportion as real data
  dplyr::slice_sample(n = round(nrow(p[which(p$swp == TRUE & p$pool == "real"), ])/5)) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  dplyr::select(val, type, year) |>
  ggplot(aes(val)) + 
  geom_bar(aes(fill = year,
               alpha = type),
           position = "fill") +
  scale_fill_manual("Year", 
                    values = okabe[1:5]) +
  scale_alpha_manual("Type", values = c(0.8, 0.3)) +
  facet_wrap(~ year, ncol = 1) +
  labs(title = "Primary Species - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Primary Species",
       y = "Proportion",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()



## Secondary Species Composition ####

# Note VRI fixes done at the start of this section + start of document

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$SPECIES_CD_2,
                      "type" = "Random Secondary Spp",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP),
           data.frame("val" = elk$species_cd_2,
                      "type" = "Elk Secondary Spp",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP))

# Remove NA
p <- p[!is.na(p$val), ]

# STACKED BAR - Random vs Selected
p |> 
  dplyr::group_by(val) |>
  dplyr::mutate(n = dplyr::n()) |> 
  dplyr::filter(n > 1000) |>
  ggplot(aes(val)) + 
  geom_bar(aes(fill = type),
           alpha = 0.7) +
  scale_fill_manual(values = c("#E69F00", "#009E73"),
                    name = "Type",
                    #labels = c()
  ) +
  labs(title = "Secondary Species - available vs elk",
       subtitle = "All dates",
       x = "Secondary Species",
       y = "Proportion",
       caption = paste0("N data points real data = ", 
                        nrow(p[p$pool == "real data",]),
                        "\nN data points random data = ", 
                        nrow(p[p$pool != "real data",]))
  ) +
  theme_minimal()

# PROPORTIONAL BAR - Random vs Selected
p |> 
  dplyr::group_by(val) |>
  dplyr::mutate(n = dplyr::n()) |> 
  dplyr::filter(n > 1000) |>
  ggplot(aes(val)) + 
  geom_bar(aes(fill = type),
           position = "fill",
           alpha = 0.7) +
  scale_fill_manual(values = c("#E69F00", "#009E73"),
                    name = "Type",
                    #labels = c()
  ) +
  labs(title = "Secondary Species - available vs elk",
       subtitle = "All dates",
       x = "Secondary Species",
       y = "Proportion",
       caption = paste0("N data points real data = ", 
                        nrow(p[p$pool == "real data",]),
                        "\nN data points random data = ", 
                        nrow(p[p$pool != "real data",]))
  ) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

p <- p |> dplyr::mutate(val = dplyr::if_else(val %in% c("HW", "BA", "FDC", "CW",
                                                        "YC", "HM", "PW", "SS",
                                                        "DR", "BG", "MB"),
                                             val,
                                             "Other")) |>
  dplyr::mutate(val = factor(val, c("HW", "BA", "FDC", "CW",
                                    "YC", "HM", "PW", "SS",
                                    "DR", "BG", "MB",
                                    "Other")))

# SEASONAL MOSAIC
p |> 
  dplyr::filter(pool %in% c("real")) |> 
  dplyr::mutate(type = dplyr::if_else(pool == "real",
                                      season,
                                      type)) |>
  dplyr::select(val, type) |>
  dplyr::filter(!is.na(type)) |>
  ggplot() +
  geom_mosaic(aes(x = product(val),
                  fill = type)) +
  scale_fill_manual("Season",
                    values = okabe[1:3]) +
  #scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7")) +
  #khroma::scale_fill_okabeito(black_position = "last") +
  labs(x = "Secondary Species",
       y = "Season") +
  coord_flip() +
  theme_mosaic()


# PROPORTIONAL BAR - Seasonal Random vs Selected
p |> 
  dplyr::filter(!is.na(season)) |> 
  ggplot(aes(val)) + 
  geom_bar(aes(fill = type),
           position = "fill",
           alpha = 0.7) +
  facet_wrap(~ season, nrow = 3) +
  labs(title = "Primary Species - available vs elk",
       subtitle = "All dates",
       x = "Secondary Speices",
       y = "Proportion",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Spring
s1 <- p |> 
  dplyr::filter(season == "Spring") |> 
  ggplot(aes(val)) + 
  geom_bar(aes(alpha = type),
           position = "fill",
           fill = okabe[1]) +
  scale_alpha_manual("Type", values = c(0.8, 0.3)) +
  facet_wrap(~ season, nrow = 3) +
  labs(y = "Proportion") +
  theme_minimal()

# Summer
s2 <- p |> 
  dplyr::filter(season == "Summer") |> 
  ggplot(aes(val)) + 
  geom_bar(aes(alpha = type),
           position = "fill",
           fill = okabe[2]) +
  scale_alpha_manual("Type", values = c(0.8, 0.3)) +
  facet_wrap(~ season, nrow = 3) +
  labs(y = "Proportion") +
  theme_minimal()

# Winter
s3 <- p |> 
  dplyr::filter(season == "Winter") |> 
  ggplot(aes(val)) + 
  geom_bar(aes(alpha = type),
           position = "fill",
           fill = okabe[3]) +
  scale_alpha_manual("Type", values = c(0.8, 0.3)) +
  facet_wrap(~ season, nrow = 3) +
  labs(x = "Secondary Species",
       y = "Proportion") +
  theme_minimal()


ggpubr::ggarrange(s1, s2, s3, ncol = 1)


# SWP plots

# MOSAIC - Yearly SWP variation
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::select(val, year) |>
  ggplot() +
  geom_mosaic(aes(x = product(val),
                  fill = year)) +
  scale_fill_manual("Year",
                    values = okabe[1:5]) +
  labs(x = "Secondary Species",
       y = "Year",
       caption = "2021-2022 was the 'Severe' year.") +
  coord_flip() +
  theme_mosaic()

# PROPORTIONAL AREA - Yearly SWP
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Filter down to same proportion as real data
  dplyr::slice_sample(n = round(nrow(p[which(p$swp == TRUE & p$pool == "real"), ])/5)) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  dplyr::select(val, type, year) |>
  ggplot(aes(val)) + 
  geom_bar(aes(fill = year,
               alpha = type),
           position = "fill") +
  scale_fill_manual("Year", 
                    values = okabe[1:5]) +
  scale_alpha_manual("Type", values = c(0.8, 0.3)) +
  facet_wrap(~ year, ncol = 1) +
  labs(title = "Secondary Species - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Secondary Species",
       y = "Proportion",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()



# DISTURBANCE -------------------------------------------------------------


## Disturbance Year ####

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$disturbance_year,
                      "type" = "Random dist. yr",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP),
           data.frame("val" = elk$disturbance_year,
                      "type" = "Elk dist. yr",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP))

# DENSITY PLOT - Random vs Selected
p |>
  ggplot(aes(x = val)) +
  geom_density(aes(color = type,
                   fill = type),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Type",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Type",
                     #labels = c()
  ) +
  labs(title = "Disturbance Year - available vs elk",
       subtitle = "All dates",
       x = "Disturbance Year",
       y = "Density",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(season %in% c("Winter", "Spring", "Summer")) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              pch = ".",
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk dist. yr", "Random dist. yr")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 2026) +
  geom_text(aes(label = N),
            nudge_y = 10) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Disturbance Year") +
  theme_minimal()



## DENSITY PLOT
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = dplyr::if_else(pool == "real",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Disturbance Year",
       y = "Density") +
  theme_minimal()


p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(pool == "real") |>
  units::drop_units() |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Disturbance Year",
       y = "Density") +
  theme_minimal()


## DENSITY PLOT SPLIT BY SEASON
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season,
             linetype = type)) +
  geom_density(alpha = 0.2) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  scale_x_continuous(limits = c(1600, 2025)) +
  facet_wrap(~ season) +
  labs(x = "Disturbance Year",
       y = "Density",
       caption = "100k random data points for each season are sampled from that season's MCP. \nSeasonal MCPs are derived by merging all the MCPs from each elk-season into a single polygon.") +
  theme_minimal()



# SWP plots

## BOXPLOT - Yearly variation WITHOUT random
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Disturbance Year - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Disturbance Year",
       caption = "2021-2022 was the 'Severe' year.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 2050,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 2100,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 2150,
              color = "black") +
  theme_minimal()


## BOXPLOT - Yearly variation WITH RANDOM
p |>
  dplyr::filter(swp == TRUE) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(year = dplyr::if_else(pool == "random", "Random", year)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:6],
                     name = "Type") +
  labs(title = "Disturbance Year - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Disturbance Year",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 2050,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 2100,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 2150,
              color = "black") +
  theme_minimal()


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = val,
             color = as.factor(year))) +
  geom_density(aes(color = year,
                   fill = year),
               alpha = 0.1) +
  scale_fill_manual(values = okabe[1:5],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:5],
                     name = "Year",
                     #labels = c()
  ) +
  labs(title = "Disturbance Year - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Disturbance Year",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()



## SWP ACTUAL VS RANDOM
# Random SWP points are sampled from the the pooled Winter MCPs
# of elk individuals that have experienced a severe winter.
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  #units::drop_units() |>
  ggplot() +
  geom_density(aes(x = val,
                   color = year,
                   fill = year,
                   linetype = type),
               alpha = 0.1) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_fill_manual(values = okabe[1:7],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:7],
                     name = "Year",
                     #labels = c()
  ) +
  facet_wrap(~ year) +
  labs(title = "Disturbance Year - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Disturbance Year",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()


## Edginess ####

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$edginess,
                      "type" = "Random Edginess",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP),
           data.frame("val" = elk$edginess,
                      "type" = "Elk Edginess",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP))

# DENSITY PLOT - Random vs Selected
p |>
  ggplot(aes(x = val)) +
  geom_density(aes(color = type,
                   fill = type),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Type",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Type",
                     #labels = c()
  ) +
  labs(title = "Edginess - available vs elk",
       subtitle = "All dates",
       x = "Edginess",
       y = "Density",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(season %in% c("Winter", "Spring", "Summer")) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              pch = ".",
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk Edginess", "Random Edginess")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 90) +
  geom_text(aes(label = N),
            nudge_y = 10) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Edginess") +
  theme_minimal()



## DENSITY PLOT
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = dplyr::if_else(pool == "real",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Edginess",
       y = "Density") +
  theme_minimal()


p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(pool == "real") |>
  units::drop_units() |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Edginess",
       y = "Density") +
  theme_minimal()


## DENSITY PLOT SPLIT BY SEASON
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season,
             linetype = type)) +
  geom_density(alpha = 0.2) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  facet_wrap(~ season) +
  labs(x = "Edginess",
       y = "Density",
       caption = "100k random data points for each season are sampled from that season's MCP. \nSeasonal MCPs are derived by merging all the MCPs from each elk-season into a single polygon.") +
  theme_minimal()



# SWP plots

## BOXPLOT - Yearly variation WITHOUT random
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Edginess - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Edginess",
       caption = "2021-2022 was the 'Severe' year.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 92,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 96,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 100,
              color = "black") +
  theme_minimal()


## BOXPLOT - Yearly variation WITH RANDOM
p |>
  dplyr::filter(swp == TRUE) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(year = dplyr::if_else(pool == "random", "Random", year)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:6],
                     name = "Type") +
  labs(title = "Edginess - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Edginess",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 92,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 96,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 100,
              color = "black") +
  theme_minimal()


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = val,
             color = as.factor(year))) +
  geom_density(aes(color = year,
                   fill = year),
               alpha = 0.1) +
  scale_fill_manual(values = okabe[1:5],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:5],
                     name = "Year",
                     #labels = c()
  ) +
  labs(title = "Edginess - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Edginess",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()



## SWP ACTUAL VS RANDOM
# Random SWP points are sampled from the the pooled Winter MCPs
# of elk individuals that have experienced a severe winter.
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  #units::drop_units() |>
  ggplot() +
  geom_density(aes(x = val,
                   color = year,
                   fill = year,
                   linetype = type),
               alpha = 0.1) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_fill_manual(values = okabe[1:7],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:7],
                     name = "Year",
                     #labels = c()
  ) +
  facet_wrap(~ year) +
  labs(title = "Edginess - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Edginess",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()


## Edge Distance ####

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$edge_dist_m,
                      "type" = "Random Edge Distance",
                      "season" = random_pts$season,
                      "pool" = "random",
                      "dttm" = NA,
                      "year" = NA,
                      "yday" = random_pts$yday,
                      "swp" = random_pts$SWP),
           data.frame("val" = elk$edge_dist_m,
                      "type" = "Elk Edge Distance",
                      "season" = elk$season,
                      "pool" = "real",
                      "dttm" = elk$dttm,
                      "year" = elk$year,
                      "yday" = elk$yday,
                      "swp" = elk$SWP))

# DENSITY PLOT - Random vs Selected
p |>
  ggplot(aes(x = val)) +
  geom_density(aes(color = type,
                   fill = type),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Type",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Type",
                     #labels = c()
  ) +
  labs(title = "Edge Distance - available vs elk",
       subtitle = "All dates",
       x = "Edge Distance",
       y = "Density",
       caption = paste0("A subsample of 100k random points are drawn from the sum of all seasonal MCPs.
                        N elk pts = ", nrow(p[p$pool == "real",]),
                        "\nN random pts = ", nrow(p[p$pool == "random",]))) +
  theme_minimal()

# Seasonal differences

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(season %in% c("Winter", "Spring", "Summer")) |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              pch = ".",
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk Edge Distance", "Random Edge Distance")),
              test = "wilcox.test",
              map_signif_level = TRUE) +
  geom_text(aes(label = N),
            nudge_y = 10) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Edge Distance") +
  theme_minimal()



## DENSITY PLOT
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = dplyr::if_else(pool == "real",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Edge Distance",
       y = "Density") +
  theme_minimal()


p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::filter(pool == "real") |>
  units::drop_units() |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Edge Distance",
       y = "Density") +
  theme_minimal()


## DENSITY PLOT SPLIT BY SEASON
p |>
  dplyr::filter(!is.na(season)) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = val,
             color = season,
             fill = season,
             linetype = type)) +
  geom_density(alpha = 0.2) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  facet_wrap(~ season) +
  labs(x = "Edge Distance",
       y = "Density",
       caption = "100k random data points for each season are sampled from that season's MCP. \nSeasonal MCPs are derived by merging all the MCPs from each elk-season into a single polygon.") +
  theme_minimal()



# SWP plots

## BOXPLOT - Yearly variation WITHOUT random
p |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Edge Distance - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Edge Distance",
       caption = "2021-2022 was the 'Severe' year.") +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 1300,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 1400,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 1500,
              color = "black") +
  theme_minimal()


## BOXPLOT - Yearly variation WITH RANDOM
p |>
  dplyr::filter(swp == TRUE) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(year = dplyr::if_else(pool == "random", "Random", year)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val)) +
  geom_jitter(aes(color = as.factor(year)),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:6],
                     name = "Type") +
  labs(title = "Edge Distance - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Edge Distance",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  # geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
  #                                c("2021-2022", "2020-2021"),
  #                                c("2021-2022", "2022-2023"),
  #                                c("2022-2023", "2023-2024")),
  #             map_signif_level = TRUE,
  #             test = "wilcox.test",
  #             color = "black") +
  # geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
  #                                c("2021-2022", "2023-2024")),
  #             map_signif_level = TRUE,
  #             test = "wilcox.test",
  #             y_position = 1300,
  #             color = "black") +
  # geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
  #             map_signif_level = TRUE,
  #             test = "wilcox.test",
  #             y_position = 1400,
  #             color = "black") +
  # geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
  #             map_signif_level = TRUE,
  #             test = "wilcox.test",
  #             y_position = 1500,
  #             color = "black") +
  theme_minimal()


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(pool == "real") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = val,
             color = as.factor(year))) +
  geom_density(aes(color = year,
                   fill = year),
               alpha = 0.1) +
  scale_fill_manual(values = okabe[1:5],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:5],
                     name = "Year",
                     #labels = c()
  ) +
  labs(title = "Edge Distance - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Edge Distance",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()



## SWP ACTUAL VS RANDOM
# Random SWP points are sampled from the the pooled Winter MCPs
# of elk individuals that have experienced a severe winter.
p |>
  dplyr::filter(swp == TRUE & pool == "random") |>
  dplyr::select(val, yday, year, type, pool) |>
  # Duplicate random dataset for each year
  dplyr::mutate(year = purrr::map2(2019, 2024, `:`)) |>
  tidyr::unnest(cols = c(year)) |>
  # Bind in the real data now
  dplyr::bind_rows(p[which(p$swp == TRUE & p$pool == "real"), ]) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::filter(year %in% c("2019-2020", "2020-2021",
                            "2021-2022", "2022-2023", "2023-2024")) |>
  #units::drop_units() |>
  ggplot() +
  geom_density(aes(x = val,
                   color = year,
                   fill = year,
                   linetype = type),
               alpha = 0.1) +
  scale_linetype("Source", labels = c("Real data", "Random data")) +
  scale_fill_manual(values = okabe[1:7],
                    name = "Year",
                    #labels = c()
  ) +
  scale_color_manual(values = okabe[1:7],
                     name = "Year",
                     #labels = c()
  ) +
  facet_wrap(~ year) +
  labs(title = "Edge Distance - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Edge Distance",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.
       50k 'Random' points are drawn from the pooled Winter MCPs of elk that experienced the 2021-2022 severe winter.
       Note that the 'Winter' MCP date range runs from Jan 01-Mar 31.") +
  theme_minimal()

