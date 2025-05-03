
# SETUP -----------------------------------------------------------------

# Load libraries
library(targets)
library(sf)
library(ggplot2)
library(ggsignif)
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

# Two elk did not experience severe winter conditions, per Mario's
# work looking at snow depth data on cameras deployed across the
# study region. Remove these two elk from the severe winter
# comparison.
non_swp_elk <- c("20-1001", "20-1002")

# These are the elk IDs that specifically experienced the 2021
# severe winter (i.e. cuts out any that also weren't collared
# yet)
swp_elk <- unique(elk[["animal_id"]][lubridate::date(elk$dttm) %in% swp_dates])

# Load up targets
tar_load(elk)
tar_load(all_seasons_mcp) # Seasonal MCP ranges
tar_load(elk_dem) # TRIM DEM-derived attributes
tar_load(elk_lidar) # UWR LiDAR-derived attributes
tar_load(elk_chm) # BCTS Crown Height Model
tar_load(elk_vri) # Madrone VRI-derived attributes

#tar_load(vri) # vri polyons

elk <- janitor::remove_empty(elk)
elk <- merge(elk, elk_dem, by = "idposition", all = TRUE)
elk <- merge(elk, elk_lidar, by = "idposition", all = TRUE)
elk <- merge(elk, elk_chm, by = "idposition", all = TRUE)
elk <- merge(elk, elk_vri, by = "idposition", all = TRUE)

rm(elk_dem, elk_lidar, elk_chm, elk_vri)

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

#' Perhaps let's start with comparing the GPS collar 
#' derived elevation to the DEM elevation to the LiDAR
#' elevation (which is probably the most accurate)


# Histogram of all continuous vars
plot_histogram(elk)



## Study Area + Random pts ####

# First let's get a polygon of Vancouver Island
# Pull BC shapefile from rnaturalearth
bc <- rnaturalearth::ne_states(country = "canada")
bc <- bc[bc$name == "British Columbia", ]
bc <- st_transform(bc, 3005) # BC Albers projection
bc  <- st_cast(bc, "POLYGON") # Multipart to Singlepart
bc$area_sqkm <- sf::st_area(bc) # Recalculate area of each indiv polygon now
bc$area_sqkm <- units::set_units(bc$area_sqkm, "km2") # Convert unit to sqkm
# We know Vancouver island is approx 32,000 km2, so subset data to polygons approx that size!
vi <- bc[bc$area_sqkm > units::set_units(30000, "km2") & bc$area_sqkm < units::set_units(33000, "km2"), ]
vi <- sf::st_geometry(vi) # drop attributes - keep just the geometry

# Create a minimum convex polygon that contains all our elk
# data points
study_area <- st_convex_hull(st_union(elk))
study_area <- sf::st_buffer(study_area, dist = 10) # buffer by 10km
study_area <- sf::st_intersection(study_area, vi) # Intersect w VI to cut out water areas

# Now subsample random points within the study area
random_pts <- sf::st_sample(study_area, size = nrow(elk))
random_pts <- sf::st_as_sf(random_pts)

plot(study_area)
plot(random_pts, pch = "+", cex = 0.5, add = TRUE) # p dense
plot(elk, pch = "+", cex = 0.5, col = "red", add = TRUE)

ggplot() +
  geom_sf(data = vi) +
  geom_sf(data = study_area) +
  geom_sf(data = random_pts,
          shape = 3) +
  geom_sf(data = elk,
          shape = 3,
          color = "red") +
  theme_minimal()


# New system: for the plots comparing specific time periods,
# just use the dates + seasons assigned to the random data.
random_pts$dttm <- elk$dttm
random_pts$season <- elk$season


## Seasonal random pts ####

# Merge seasonal MCPs of all individuals into
# single large polygon for each season. These will
# be our polygons for choosing random points from
# for seasonal use vs. availability.
szn_mcp <- all_seasons_mcp |>
  dplyr::group_by(season) |>
  dplyr::summarise()

szn_mcp <- sf::st_buffer(szn_mcp, dist = 10) # buffer by 10km
szn_mcp <- sf::st_intersection(szn_mcp, vi) # Intersect w VI to cut out water areas

random_pts_szn <- lapply(unique(szn_mcp$season),
                         function(x) {
                           poly <- szn_mcp[szn_mcp$season == x, ]
                           out <- sf::st_sample(poly, size = nrow(elk[which(elk$season == x), ]))
                           out <- sf::st_as_sf(out)
                           out$season <- x
                           return(out)
                         })
random_pts_szn <- dplyr::bind_rows(random_pts_szn)

ggplot() +
  geom_sf(data = vi) +
  geom_sf(data = szn_mcp) +
  geom_sf(data = random_pts_szn,
          aes(color = season),
          shape = 3) +
  scale_color_manual(values = okabe[1:3]) +
  geom_sf(data = elk,
          shape = 3,
          color = "red") +
  coord_sf(xlim = c(954700, 1045000),
           ylim = c(509250, 600700)) +
  theme_minimal()



## SWP random pts ####

# Use the MCPs only of the elk that experienced SWP
swp_mcp <- all_seasons_mcp |>
  dplyr::filter(animal_id %in% swp_elk,
                season == "Winter") |>
  dplyr::summarise()

swp_mcp <- sf::st_buffer(swp_mcp, dist = 10) # buffer by 10km
swp_mcp <- sf::st_intersection(swp_mcp, vi) # Intersect w VI to cut out water areas

# Now subsample random points within SWP area
random_pts_swp <- sf::st_sample(swp_mcp, size = nrow(elk[which(elk$animal_id %in% swp_elk & lubridate::date(elk$dttm) %in% swp_dates), ]))
random_pts_swp <- sf::st_as_sf(random_pts_swp)
random_pts_swp$season <- "Winter"


## Merge random pts together ####

# Col to say which type of random pts it is
random_pts$pool <- "full study area"
random_pts_szn$pool <- "seasonal MCPs"
random_pts_swp$pool <- "SWP MCPs"

random_pts <- dplyr::bind_rows(random_pts, random_pts_szn, random_pts_swp)

rm(random_pts_szn, random_pts_swp)


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
  tidyr::pivot_longer(cols = 1,
                      names_to = "Source",
                      values_to = "Percent") |>
  ggplot(aes(x = Percent)) +
  geom_density(aes(color = Source,
                   fill = Source),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "Source",
                    labels = c("TRIM DEM")) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Source",
                     labels = c("TRIM DEM")) +
  scale_x_continuous(limits = c(-10, 200)) +
  labs(title = "How does the slope grade data from various sources compare?",
       x = "Slope grade (%)",
       y = "Density") +
  theme_minimal()

elk |>
  sf::st_drop_geometry() |>
  dplyr::select(slope_prct, slope_percent) |>
  dplyr::rename(slope_dem = slope_prct,
                slope_lidar = slope_percent) |>
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
                    labels = c("TRIM DEM",
                               "LiDAR")) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "Source",
                     labels = c("TRIM DEM",
                                "LiDAR")) +
  scale_x_continuous(limits = c(-10, 200)) +
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

dem <- terra::rast("GIS/DEM/CDED_VRT.vrt") # load DEM GIS data

# Extract DEM values from the random pts coordinates
random_pts <- cbind(random_pts,
                    terra::extract(dem, random_pts, ID = FALSE))

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("m" = random_pts$CDED_VRT,
                      "type" = "Random elevations",
                      "dttm" = random_pts$dttm,
                      "season" = random_pts$season,
                      "pool" = random_pts$pool),
               data.frame("m" = elk$elevation_m,
                          "type" = "Elk elevations",
                          "dttm" = elk$dttm,
                          "season" = elk$season,
                          "pool" = "real data"))

ggplot(p,
       aes(x = m)) +
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
       caption = paste0("N data points per group = ", nrow(p)/2)) +
  theme_minimal()

# Seasonal differences
# Subset p now so that random_pts is much smaller - 
# we'll subset the random points so that the sample size
# is closer to the number of seasonal days in the actual data
p <- p[!is.na(p$season), ] # remove NA season data
aggregate(m ~ season + type, p, FUN = length) # double check

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
p |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |> # calculate N points per group
  dplyr::mutate(N = paste0("N = ", as.character(N))) |> # turn it into a plot label
  dplyr::mutate(N = ifelse(m == max(m, na.rm = T),
                           N,
                           NA)) |> # now get rid of all the duplicate labels so it doesn't plot literally thousands of them. Only plot 1 label at the max value of "m" per group.
  ggplot(aes(x = type,
             y = m)) +
  geom_jitter(aes(color = season),
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
  dplyr::mutate(season = dplyr::if_else(type == "Elk elevations",
                                        season,
                                        "Random points")) |>
  dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  ggplot(aes(x = m,
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



# SWP plots
p$yday <- lubridate::yday(p$dttm)
p$year <- lubridate::year(p$dttm)


## BOXPLOT - Yearly variation
p |>
  dplyr::filter(type == "Elk elevations") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = m,
             color = as.factor(year))) +
  geom_jitter(alpha = 0.1) +
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
              color = "black") +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 1400,
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
  dplyr::filter(type == "Elk elevations") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = m,
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



## Slope % -----------------------------------------------------------------

# Available versus what elk chose
dem <- terra::rast("GIS/DEM/CDED_VRT.vrt")
dem <- terra::terrain(dem, v = "slope", unit = "radians")

# Extract DEM values from the random pts coordinates
random_pts <- cbind(random_pts,
                    terra::extract(dem, random_pts, ID = FALSE))
random_pts$slope <- tan(random_pts$slope) # Convert from degrees to %
random_pts$slope <- round(random_pts$slope * 100, 1)

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$slope,
                          "type" = "Random slopes",
                          "dttm" = random_pts$dttm,
                          "season" = random_pts$season),
               data.frame("val" = elk$slope_prct,
                          "type" = "Elk slopes",
                          "dttm" = elk$dttm,
                          "season" = elk$season))

p <- p[p$val > 0, ] # cut out water (perfectly flat)

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
  labs(title = "Slope grade - available vs elk",
       subtitle = "All dates",
       x = "Slope (%)",
       y = "Density",
       caption = paste0("N data points per group = ", nrow(p)/2)) +
  theme_minimal()

# Seasonal differences
# Subset p now so that random_pts is much smaller - 
# we'll subset the random points so that the sample size
# is closer to the number of seasonal days in the actual data
p <- p[!is.na(p$season), ] # remove NA season data
aggregate(val ~ season + type, p, FUN = length) # double check

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
p |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |> # calculate N points per group
  dplyr::mutate(N = paste0("N = ", as.character(N))) |> # turn it into a plot label
  dplyr::mutate(N = ifelse(val == max(val, na.rm = T),
                           N,
                           NA)) |> # now get rid of all the duplicate labels so it doesn't plot literally thousands of them. Only plot 1 label at the max value of "m" per group.
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
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



# SWP plots
p$yday <- lubridate::yday(p$dttm)
p$year <- lubridate::year(p$dttm)


## BOXPLOT - Yearly variation
p |>
  dplyr::filter(type == "Elk slopes") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val,
             color = as.factor(year))) +
  geom_jitter(alpha = 0.1) +
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

## DENSITY PLOT
p |>
  dplyr::filter(type == "Elk slopes") |>
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


## Slope Aspect ------------------------------------------------------------


# Available versus what elk chose

dem <- terra::rast("GIS/DEM/CDED_VRT.vrt")
dem <- terra::terrain(dem, v = "aspect", unit = "degrees")

# Extract DEM values from the random pts coordinates
random_pts <- cbind(random_pts,
                    terra::extract(dem, random_pts, ID = FALSE))

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$aspect,
                          "type" = "Random aspects",
                          "dttm" = random_pts$dttm,
                          "season" = random_pts$season,
                          "slope" = random_pts$slope),
               data.frame("val" = elk$slope_aspect,
                          "type" = "Elk aspects",
                          "dttm" = elk$dttm,
                          "season" = elk$season,
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
  labs(title = "Slope aspect - available vs elk",
       subtitle = "All dates",
       x = "Slope aspect (degrees)",
       y = "Density",
       caption = paste0("N data points per group = ", nrow(p)/2)) +
  theme_minimal()

# Seasonal differences
# Subset p now so that random_pts is much smaller - 
# we'll subset the random points so that the sample size
# is closer to the number of seasonal days in the actual data
p <- p[!is.na(p$season), ] # remove NA season data
aggregate(val ~ season + type, p, FUN = length) # double check

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
p |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |> # calculate N points per group
  dplyr::mutate(N = paste0("N = ", as.character(N))) |> # turn it into a plot label
  dplyr::mutate(N = ifelse(val == max(val, na.rm = T),
                           N,
                           NA)) |> # now get rid of all the duplicate labels so it doesn't plot literally thousands of them. Only plot 1 label at the max value of "m" per group.
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
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
  dplyr::mutate(season = dplyr::if_else(type == "Elk aspects",
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



# SWP plots
p$yday <- lubridate::yday(p$dttm)
p$year <- lubridate::year(p$dttm)


## BOXPLOT - Yearly variation
p |>
  dplyr::filter(type == "Elk aspects") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val,
             color = as.factor(year))) +
  geom_jitter(alpha = 0.1) +
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
              y_position = 400,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 410,
              color = "black") +
  theme_minimal()

## DENSITY PLOT
p |>
  dplyr::filter(type == "Elk aspects") |>
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


## Roughness ---------------------------------------------------------------

# Abandoned as these are effectively identical to slope grade.

# Available versus what elk chose
dem <- terra::rast("GIS/DEM/CDED_VRT.vrt")
dem <- terra::terrain(dem, v = "roughness")

# Extract DEM values from the random pts coordinates
random_pts <- cbind(random_pts,
                    terra::extract(dem, random_pts, ID = FALSE))

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$roughness,
                          "type" = "Random roughness",
                          "dttm" = random_pts$dttm,
                          "season" = random_pts$season,
                          "slope" = random_pts$slope),
               data.frame("val" = elk$roughness,
                          "type" = "Elk roughness",
                          "dttm" = elk$dttm,
                          "season" = elk$season,
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
       caption = paste0("N data points per group = ", nrow(p)/2)) +
  theme_minimal()

# Seasonal differences
# Subset p now so that random_pts is much smaller - 
# we'll subset the random points so that the sample size
# is closer to the number of seasonal days in the actual data
p <- p[!is.na(p$season), ] # remove NA season data
aggregate(val ~ season + type, p, FUN = length) # double check

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
p |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |> # calculate N points per group
  dplyr::mutate(N = paste0("N = ", as.character(N))) |> # turn it into a plot label
  dplyr::mutate(N = ifelse(val == max(val, na.rm = T),
                           N,
                           NA)) |> # now get rid of all the duplicate labels so it doesn't plot literally thousands of them. Only plot 1 label at the max value of "m" per group.
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
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
  dplyr::mutate(season = dplyr::if_else(type == "Elk roughness",
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
  labs(x = "Roughness",
       y = "Density") +
  theme_minimal()



# SWP plots
p$yday <- lubridate::yday(p$dttm)
p$year <- lubridate::year(p$dttm)


## BOXPLOT - Yearly variation
p |>
  dplyr::filter(type == "Elk roughness") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val,
             color = as.factor(year))) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(fill = NA) +
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
  dplyr::filter(type == "Elk roughness") |>
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

rm(dem)


# LiDAR -----------------------------------------------------------


## UWR Data dates ####

elk |>
  dplyr::filter(!is.na(canopy_height)) |>
  ggplot(aes(x = dttm, y = TRUE)) +
  geom_point() +
  theme_minimal()

## Canopy Height ####

uwr <- terra::rast(tar_read(uwr_lidar_gdb), subds = "canopy_height")
uwr <- terra::values(uwr)
uwr <- as.data.frame(uwr)
uwr$type <- "All canopy heights"
names(uwr)[1] <- "canopy_height"

uwr_p <- rbind(uwr, 
               data.frame("canopy_height" = elk$canopy_height,
                          "type" = "Elk canopy heights"))
uwr_p <- na.omit(uwr_p)


ggplot(uwr_p,
       aes(x = canopy_height)) +
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
  labs(title = "Canopy height - available vs elk",
       subtitle = "All dates",
       x = "Canopy height (m)",
       y = "Density") +
  theme_minimal()


uwr_p <- rbind(uwr, 
               data.frame("canopy_height" = elk[["canopy_height"]][lubridate::day(elk$dttm) %in% swp_days],
                          "type" = "Elk canopy heights"))

ggplot(uwr_p,
       aes(x = canopy_height)) +
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
  labs(title = "Canopy height - available vs elk",
       subtitle = "During the Severe Winter Period",
       x = "Canopy height (m)",
       y = "Density") +
  theme_minimal()


swp_vs_all <- elk[,c("dttm", "canopy_height")]
swp_vs_all$in_swp <- lubridate::day(swp_vs_all$dttm) %in% swp_days

ggplot(swp_vs_all,
       aes(x = canopy_height)) +
  geom_density(aes(color = in_swp,
                   fill = in_swp),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "In SWP?",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "In SWP?",
                     #labels = c()
  ) +
  labs(title = "Canopy height - Non-SWP vs SWP",
       x = "Canopy height (m)",
       y = "Density") +
  theme_minimal()


## Edge Category -----------------------------------------------------------

# TODO: SHOULD ACTUALLY BE A CHI SQUARE

uwr <- terra::rast(tar_read(uwr_lidar_gdb), subds = "Edge_Category")
uwr <- terra::values(uwr)
uwr <- as.data.frame(uwr)
uwr$type <- "All edge categories"
names(uwr)[1] <- "edge_category"

# Investigating in QGIS reveals this is actually two bands. We just 
# care about the categorical band, not the alpha band.
uwr <- uwr[which(uwr$edge_category %in% c(1, 2)), ]

uwr_p <- rbind(uwr, 
               data.frame("edge_category" = elk$edge_category,
                          "type" = "Elk edge categories"))
uwr_p <- na.omit(uwr_p)


chisq <- uwr_p |>
  dplyr::group_by(edge_category, type) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = edge_category,
                     values_from = n) |>
  tibble::column_to_rownames(var = "type") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  chisq.test()

corrplot::corrplot(chisq$residuals, is.corr = FALSE)


ggplot(uwr_p,
       aes(x = edge_category)) +
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
  labs(title = "Edge category - available vs elk",
       subtitle = "All dates",
       x = "Edge category",
       y = "Density") +
  theme_minimal()


uwr_p <- rbind(uwr, 
               data.frame("edge_category" = elk[["edge_category"]][lubridate::day(elk$dttm) %in% swp_days],
                          "type" = "Elk edge categories"))
uwr_p <- na.omit(uwr_p)

chisq <- uwr_p |>
  dplyr::group_by(edge_category, type) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = edge_category,
                     values_from = n) |>
  tibble::column_to_rownames(var = "type") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  chisq.test()

corrplot::corrplot(chisq$residuals, is.corr = FALSE)

ggplot(uwr_p,
       aes(x = edge_category)) +
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
  labs(title = "Edge category - available vs elk",
       subtitle = "During the Severe Winter Period",
       x = "Edge category",
       y = "Density") +
  theme_minimal()


swp_vs_all <- elk[,c("dttm", "edge_category")]
swp_vs_all$in_swp <- lubridate::day(swp_vs_all$dttm) %in% swp_days
swp_vs_all <- sf::st_drop_geometry(swp_vs_all)
swp_vs_all <- na.omit(swp_vs_all)

chisq <- swp_vs_all |>
  dplyr::group_by(edge_category, in_swp) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = edge_category,
                     values_from = n) |>
  tibble::column_to_rownames(var = "in_swp") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  chisq.test()

corrplot::corrplot(chisq$residuals, is.corr = FALSE)

ggplot(swp_vs_all,
       aes(x = edge_category)) +
  geom_density(aes(color = in_swp,
                   fill = in_swp),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "In SWP?",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "In SWP?",
                     #labels = c()
  ) +
  labs(title = "Edge category - Non-SWP vs SWP",
       x = "Edge category",
       y = "Density") +
  theme_minimal()


## Edge Distance -----------------------------------------------------------


uwr <- terra::rast(tar_read(uwr_lidar_gdb), subds = "Edge_Distance_LiDAR")
uwr <- terra::values(uwr)
uwr <- as.data.frame(uwr)
uwr$type <- "All edge distances"
names(uwr)[1] <- "edge_dist"

uwr_p <- rbind(uwr, 
               data.frame("edge_dist" = elk$edge_dist,
                          "type" = "Elk edge distances"))
uwr_p <- na.omit(uwr_p)


ggplot(uwr_p,
       aes(x = edge_dist)) +
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
  labs(title = "Edge distance - available vs elk",
       subtitle = "All dates",
       x = "Edge distance",
       y = "Density") +
  theme_minimal()


uwr_p <- rbind(uwr, 
               data.frame("edge_dist" = elk[["edge_dist"]][lubridate::day(elk$dttm) %in% swp_days],
                          "type" = "Elk edge distances"))

ggplot(uwr_p,
       aes(x = edge_dist)) +
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
  labs(title = "Edge distance - available vs elk",
       subtitle = "During the Severe Winter Period",
       x = "Edge distance",
       y = "Density") +
  theme_minimal()


swp_vs_all <- elk[,c("dttm", "edge_dist")]
swp_vs_all$in_swp <- lubridate::day(swp_vs_all$dttm) %in% swp_days

ggplot(swp_vs_all,
       aes(x = edge_dist)) +
  geom_density(aes(color = in_swp,
                   fill = in_swp),
               alpha = 0.1) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                    name = "In SWP?",
                    #labels = c()
  ) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
                     name = "In SWP?",
                     #labels = c()
  ) +
  labs(title = "Edge distance - all dates vs severe winter period",
       x = "Edge distance",
       y = "Density") +
  theme_minimal()



rm(chisq, uwr, uwr_p)



# CHM ---------------------------------------------------------------------


## CHM Data dates ####

elk |>
  dplyr::filter(!is.na(crown_height)) |>
  ggplot(aes(x = dttm, y = TRUE)) +
  geom_point() +
  theme_minimal()

# % data points with BCTS Crown Height data
round(sum(!is.na(elk$crown_height)) / nrow(elk) * 100)

unique(sf::st_drop_geometry(elk[["animal_id"]][!is.na(elk$crown_height)])) |>
  length()

## Crown Height ####

# Available versus what elk chose
chm <- terra::rast("GIS/LiDAR products/crown_height.tif")

# Extract DEM values from the random pts coordinates
random_pts <- cbind(random_pts,
                    terra::extract(chm, random_pts, ID = FALSE))

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$crown_height,
                      "type" = "Random crown height",
                      "dttm" = random_pts$dttm,
                      "season" = random_pts$season),
           data.frame("val" = elk$crown_height,
                      "type" = "Elk crown height",
                      "dttm" = elk$dttm,
                      "season" = elk$season))

p <- p[!is.na(p$val), ] # cut out NA areas - we don't have full LiDAR coverage

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
  labs(title = "Slope grade - available vs elk",
       subtitle = "All dates",
       x = "Slope (%)",
       y = "Density",
       caption = paste0("N elk data points = ", nrow(p[p$type == "Elk crown height", ]), "; N random data points = ", nrow(p[p$type == "Random crown height", ]))) +
  theme_minimal()

# Seasonal differences
# Subset p now so that random_pts is much smaller - 
# we'll subset the random points so that the sample size
# is closer to the number of seasonal days in the actual data
p <- p[!is.na(p$season), ] # remove NA season data
aggregate(val ~ season + type, p, FUN = length) # double check

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
p |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |> # calculate N points per group
  dplyr::mutate(N = paste0("N = ", as.character(N))) |> # turn it into a plot label
  dplyr::mutate(N = ifelse(val == max(val, na.rm = T),
                           N,
                           NA)) |> # now get rid of all the duplicate labels so it doesn't plot literally thousands of them. Only plot 1 label at the max value of "m" per group.
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk crown height", "Random crown height")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 80) +
  geom_text(aes(label = N),
            nudge_y = 10) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Crown Height (m)",
       caption = "Differing sample sizes within seasons are due to uneven BCTS LiDAR coverage between where elk vs. random points may have been.") +
  theme_minimal()


## DENSITY PLOT
p |>
  dplyr::mutate(season = dplyr::if_else(type == "Elk crown height",
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
  labs(x = "Crown Height (m)",
       y = "Density") +
  theme_minimal()


p |>
  dplyr::filter(type == "Elk crown height") |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  labs(x = "Crown Height (m)",
       y = "Density") +
  theme_minimal()



# SWP plots
p$yday <- lubridate::yday(p$dttm)
p$year <- lubridate::year(p$dttm)


## BOXPLOT - Yearly variation
p |>
  dplyr::filter(type == "Elk crown height") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val,
             color = as.factor(year))) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Crown Height - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Crown Height (m)",
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
              y_position = 60,
              color = "black") +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 70,
              color = "black") +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              y_position = 80,
              color = "black") +
  theme_minimal()

## DENSITY PLOT
p |>
  dplyr::filter(type == "Elk crown height") |>
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
  labs(title = "Crown Height - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Crown Height (m)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()



# VRI ---------------------------------------------------------------------

## Random VRI pts setup ####

tar_load(vri)

# Subset to cols of interest
vri <- vri[, c("Shape_Area",
               "Shape_Length",
               "INTERPRETATION_DATE", 
               "REFERENCE_YEAR", 
               "ATTRIBUTION_BASE_DATE", 
               "PROJECTED_DATE", 
               "HARVEST_DATE", 
               "Disturbance_Start_Date", 
               "Disturbance_End_Date", 
               "Harvest_Year", 
               "PROJ_AGE_1", 
               "NEW_VRI_CC_RES_AGE", 
               "BEST_AGE_CL_STS", 
               "PROJ_HEIGHT_1",
               "VRI_LIVE_STEMS_PER_HA",
               "CROWN_CLOSURE",
               "SHRUB_HEIGHT",
               "SHRUB_CROWN_CLOSURE",
               "Creation_Date")]

# Deal with slivers
vri$Shape_Area <- sf::st_area(vri)
vri$Shape_Area <- units::set_units(vri$Shape_Area, "ha")

plot(density(vri$Shape_Area))

# Polygon area is messed up. The VRI has millions of sliver polygons 
# that skew patch size. Delete them
vri <- vri[vri$Shape_Area > units::set_units(100, "m2"), ] # delete anything less than 100 sq m
plot(density(vri$Shape_Area))

# Next step - remove any polygons where the length:area ratio is totally
# whack. Visual inspection in QGIS shows deleting anything w ratio >2000 
# is appropriate
vri$len_ar_ratio <- vri$Shape_Length / vri$Shape_Area
vri$len_ar_ratio <- units::drop_units(vri$len_ar_ratio)
plot(density(vri$len_ar_ratio))
plot(density(vri$len_ar_ratio), xlim = c(1000, 5000)) # zoom in to try and find inflection point

vri <- vri[vri$len_ar_ratio < 2000, ]
hist(vri$Shape_Area, xlim = c(0, 50), breaks = 1000)

# Now merge random points w VRI.
random_pts <- sf::st_intersection(random_pts, vri)

# Remove VRI and clean up memory
rm(vri)
gc()


## Patch Size ####

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$Shape_Area,
                      "type" = "Random patch sizes",
                      "dttm" = random_pts$dttm,
                      "season" = random_pts$season),
           data.frame("val" = elk$shape_area,
                      "type" = "Elk patch sizes",
                      "dttm" = elk$dttm,
                      "season" = elk$season))

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
  labs(title = "Patch Size - available vs elk",
       subtitle = "All dates",
       x = "Patch Size (ha)",
       y = "Density",
       caption = paste0("N data points per group = ", nrow(p)/2)) +
  theme_minimal()

# Seasonal differences
# Subset p now so that random_pts is much smaller - 
# we'll subset the random points so that the sample size
# is closer to the number of seasonal days in the actual data
p <- p[!is.na(p$season), ] # remove NA season data
aggregate(val ~ season + type, p, FUN = length) # double check

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
# Better way of doing this than before...
# TODO: fix the DEM boxplots with multiple labels issues
n_label <- p |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk patch sizes", "Random patch sizes")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 980) +
  geom_text(aes(label = N),
            nudge_y = 10) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Patch size (ha)",
       caption = "Differing sample sizes between groups are caused in cases where a GPS point \nmay have landed on an empty space where a deleted sliver polygon may have been.") +
  theme_minimal()



## DENSITY PLOT
p |>
  dplyr::mutate(season = dplyr::if_else(type == "Elk patch sizes",
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
  labs(x = "Patch Size (ha)",
       y = "Density") +
  theme_minimal()


p |>
  dplyr::filter(type == "Elk patch sizes") |>
  units::drop_units() |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  scale_x_continuous(limits = c(0, 150)) +
  labs(x = "Patch Size (ha)",
       y = "Density") +
  theme_minimal()



# SWP plots
p$yday <- lubridate::yday(p$dttm)
p$year <- lubridate::year(p$dttm)


## BOXPLOT - Yearly variation
p |>
  units::drop_units() |>
  dplyr::filter(type == "Elk patch sizes") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val,
             color = as.factor(year))) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual(values = okabe[1:5],
                     name = "Type") +
  labs(title = "Patch Size - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Year",
       y = "Patch Size (ha)",
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


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(type == "Elk patch sizes") |>
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
  labs(title = "Patch Size - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Patch Size (ha)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()


## Stand Age ####


# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$NEW_VRI_CC_RES_AGE,
                      "type" = "Random stand age",
                      "dttm" = random_pts$dttm,
                      "season" = random_pts$season),
           data.frame("val" = elk$new_vri_cc_res_age,
                      "type" = "Elk stand age",
                      "dttm" = elk$dttm,
                      "season" = elk$season))

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
  labs(title = "Stand Age - available vs elk",
       subtitle = "All dates",
       x = "Stand Age",
       y = "Density",
       caption = paste0("N data points per group = ", nrow(p)/2)) +
  theme_minimal()

# Seasonal differences
# Subset p now so that random_pts is much smaller - 
# we'll subset the random points so that the sample size
# is closer to the number of seasonal days in the actual data
p <- p[!is.na(p$season), ] # remove NA season data
aggregate(val ~ season + type, p, FUN = length) # double check

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
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
       y = "Stand Age",
       caption = "Differing sample sizes between groups are caused in cases where a GPS point \nmay have landed on an empty space where a deleted sliver polygon may have been.") +
  theme_minimal()



## DENSITY PLOT
p |>
  dplyr::mutate(season = dplyr::if_else(type == "Elk stand age",
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
  dplyr::filter(type == "Elk stand age") |>
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



# SWP plots
p$yday <- lubridate::yday(p$dttm)
p$year <- lubridate::year(p$dttm)


## BOXPLOT - Yearly variation
p |>
  units::drop_units() |>
  dplyr::filter(type == "Elk stand age") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val,
             color = as.factor(year))) +
  geom_jitter(alpha = 0.1) +
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


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(type == "Elk stand age") |>
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



## Stand Height ####


# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$PROJ_HEIGHT_1,
                      "type" = "Random stand height",
                      "dttm" = random_pts$dttm,
                      "season" = random_pts$season),
           data.frame("val" = elk$proj_height_1,
                      "type" = "Elk stand height",
                      "dttm" = elk$dttm,
                      "season" = elk$season))

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
  labs(title = "Stand Height - available vs elk",
       subtitle = "All dates",
       x = "Stand Height (m)",
       y = "Density",
       caption = paste0("N data points per group = ", nrow(p)/2)) +
  theme_minimal()

# Seasonal differences
# Subset p now so that random_pts is much smaller - 
# we'll subset the random points so that the sample size
# is closer to the number of seasonal days in the actual data
p <- p[!is.na(p$season), ] # remove NA season data
aggregate(val ~ season + type, p, FUN = length) # double check

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
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
       y = "Stand Height (m)",
       caption = "Differing sample sizes between groups are caused in cases where a GPS point \nmay have landed on an empty space where a deleted sliver polygon may have been.") +
  theme_minimal()



## DENSITY PLOT
p |>
  dplyr::mutate(season = dplyr::if_else(type == "Elk stand height",
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
  dplyr::filter(type == "Elk stand height") |>
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



# SWP plots
p$yday <- lubridate::yday(p$dttm)
p$year <- lubridate::year(p$dttm)


## BOXPLOT - Yearly variation
p |>
  dplyr::filter(type == "Elk stand height") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val,
             color = as.factor(year))) +
  geom_jitter(alpha = 0.1) +
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


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(type == "Elk stand height") |>
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



## Stem Density ####

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$VRI_LIVE_STEMS_PER_HA,
                      "type" = "Random stem density",
                      "dttm" = random_pts$dttm,
                      "season" = random_pts$season),
           data.frame("val" = elk$vri_live_stems_per_ha,
                      "type" = "Elk stem density",
                      "dttm" = elk$dttm,
                      "season" = elk$season))

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
  labs(title = "Live Stem Density - available vs elk",
       subtitle = "All dates",
       x = "Live Stem Density (stems/ha)",
       y = "Density",
       caption = paste0("N data points per group = ", nrow(p)/2)) +
  theme_minimal()

# Seasonal differences
# Subset p now so that random_pts is much smaller - 
# we'll subset the random points so that the sample size
# is closer to the number of seasonal days in the actual data
p <- p[!is.na(p$season), ] # remove NA season data
aggregate(val ~ season + type, p, FUN = length) # double check

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
              alpha = 0.1) +
  geom_boxplot(fill = NA) +
  scale_color_manual("Season", values = okabe[1:4]) +
  geom_signif(comparisons = list(c("Elk stem density", "Random stem density")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 30000) +
  geom_text(aes(label = N),
            nudge_y = 10) +
  facet_wrap(~ season) +
  labs(x = "",
       y = "Live Stem Density (stems/ha)",
       caption = "Differing sample sizes between groups are caused in cases where a GPS point \nmay have landed on an empty space where a deleted sliver polygon may have been.") +
  theme_minimal()



## DENSITY PLOT
p |>
  dplyr::mutate(season = dplyr::if_else(type == "Elk stem density",
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


p |>
  dplyr::filter(type == "Elk stem density") |>
  # dplyr::mutate(season = dplyr::if_else(type == "Elk stem density",
  #                                       season,
  #                                       "Random points")) |>
  # dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Winter", "Random points"))) |>
  units::drop_units() |>
  ggplot(aes(x = val,
             color = season,
             fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_manual("Season",
                     values = okabe[1:4]) +
  scale_fill_manual("Season",
                    values = okabe[1:4]) +
  scale_x_continuous(limits = c(0, 10000)) +
  labs(x = "Live Stem Density (stems/ha)",
       y = "Density") +
  theme_minimal()



# SWP plots
p$yday <- lubridate::yday(p$dttm)
p$year <- lubridate::year(p$dttm)


## BOXPLOT - Yearly variation
p |>
  dplyr::filter(type == "Elk stem density") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val,
             color = as.factor(year))) +
  geom_jitter(alpha = 0.1) +
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


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(type == "Elk stem density") |>
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


## Crown Closure ####


# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$CROWN_CLOSURE,
                      "type" = "Random cc",
                      "dttm" = random_pts$dttm,
                      "season" = random_pts$season),
           data.frame("val" = elk$crown_closure,
                      "type" = "Elk cc",
                      "dttm" = elk$dttm,
                      "season" = elk$season))

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
  labs(title = "Crown Closure - available vs elk",
       subtitle = "All dates",
       x = "Crown Closure (%)",
       y = "Density",
       caption = paste0("N data points per group = ", round(nrow(p)/2))) +
  theme_minimal()

# Seasonal differences
# Subset p now so that random_pts is much smaller - 
# we'll subset the random points so that the sample size
# is closer to the number of seasonal days in the actual data
p <- p[!is.na(p$season), ] # remove NA season data
aggregate(val ~ season + type, p, FUN = length) # double check

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
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
  dplyr::mutate(season = dplyr::if_else(type == "Elk cc",
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



# SWP plots
p$yday <- lubridate::yday(p$dttm)
p$year <- lubridate::year(p$dttm)


## BOXPLOT - Yearly variation
p |>
  dplyr::filter(type == "Elk cc") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val,
             color = as.factor(year))) +
  geom_jitter(alpha = 0.1) +
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


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(type == "Elk cc") |>
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


## Shrub Height ####

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$SHRUB_HEIGHT,
                      "type" = "Random shrub height",
                      "dttm" = random_pts$dttm,
                      "season" = random_pts$season),
           data.frame("val" = elk$shrub_height,
                      "type" = "Elk shrub height",
                      "dttm" = elk$dttm,
                      "season" = elk$season))

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
  labs(title = "Shrub Height - available vs elk",
       subtitle = "All dates",
       x = "Shrub Height (m)",
       y = "Density",
       caption = paste0("N data points per group = ", nrow(p)/2)) +
  theme_minimal()

# Seasonal differences
# Subset p now so that random_pts is much smaller - 
# we'll subset the random points so that the sample size
# is closer to the number of seasonal days in the actual data
p <- p[!is.na(p$season), ] # remove NA season data
aggregate(val ~ season + type, p, FUN = length) # double check

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
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



# SWP plots
p$yday <- lubridate::yday(p$dttm)
p$year <- lubridate::year(p$dttm)


## BOXPLOT - Yearly variation
p |>
  dplyr::filter(type == "Elk shrub height") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val,
             color = as.factor(year))) +
  geom_jitter(alpha = 0.1) +
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


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(type == "Elk shrub height") |>
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
  labs(title = "Shrub Height - SWP across years",
       subtitle = "During the Severe Winter Period (18 Dec - 14 Jan)",
       x = "Shrub Height (m)",
       y = "Density",
       caption = "2021-2022 was the 'Severe' year.") +
  theme_minimal()



## Shrub Crown Closure ####

# Create a df of data to plot - our elk data
# merged with the random sampling data
p <- rbind(data.frame("val" = random_pts$SHRUB_CROWN_CLOSURE,
                      "type" = "Random shrub cc",
                      "dttm" = random_pts$dttm,
                      "season" = random_pts$season),
           data.frame("val" = elk$shrub_crown_closure,
                      "type" = "Elk shrub cc",
                      "dttm" = elk$dttm,
                      "season" = elk$season))

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
  labs(title = "Shrub Crown Closure - available vs elk",
       subtitle = "All dates",
       x = "Shrub Crown Closure (%)",
       y = "Density",
       caption = paste0("N data points per group = ", nrow(p)/2)) +
  theme_minimal()

# Seasonal differences
# Subset p now so that random_pts is much smaller - 
# we'll subset the random points so that the sample size
# is closer to the number of seasonal days in the actual data
p <- p[!is.na(p$season), ] # remove NA season data
aggregate(val ~ season + type, p, FUN = length) # double check

# Set factor for plotting
p$season <- factor(p$season, levels = c("Spring", "Summer", "Winter"))

## BOXPLOT
n_label <- p |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |>
  dplyr::group_by(season, type) |>
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::slice_max(val) |>
  dplyr::select(season, type, val, N) |>
  dplyr::mutate(N = paste0("N = ", N)) |>
  dplyr::distinct()

p |>
  dplyr::filter(!is.na(val)) |>
  units::drop_units() |> 
  dplyr::bind_rows(n_label) |>
  ggplot(aes(x = type,
             y = val)) +
  geom_jitter(aes(color = season),
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



# SWP plots
p$yday <- lubridate::yday(p$dttm)
p$year <- lubridate::year(p$dttm)


## BOXPLOT - Yearly variation
p |>
  dplyr::filter(type == "Elk shrub cc") |>
  dplyr::filter(yday %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(yday < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year),
             y = val,
             color = as.factor(year))) +
  geom_jitter(alpha = 0.1) +
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


## DENSITY PLOT
p |>
  units::drop_units() |>
  dplyr::filter(type == "Elk shrub cc") |>
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



