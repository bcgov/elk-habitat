# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # for tar_map()

# Set target options:
tar_option_set(
  packages = c("sf",
               "ggplot2") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

#### STATIC VARIABLES ####

# Seasons
winter <- c("01-01", "03-31") # month-day format
spring <- c("04-01", "05-15") # month-day format
summer <- c("07-01", "08-31") # month-day format

# Set max date for the study overall - we only want to use detections
# up to March 31 2024.
cutoff_date <- "2024-04-01"

# Two elk did not experience severe winter conditions, per Mario's
# work looking at snow depth data on cameras deployed across the
# study region. Remove these two elk from the severe winter
# comparison.
non_swp_elk <- c("20-1001", "20-1002")

# Severe winter period was 18 Dec 2021 - 14 Jan 2022
#lubridate::isoweek("2021-12-18")
#lubridate::isoweek("2022-01-14")
swp_weeks <- c(50, 51, 52, 53, 1, 2) # weeks 51 thru 1

#lubridate::yday("2021-12-18")
#lubridate::yday("2022-01-14")
swp_days <- c(lubridate::yday("2021-12-18"):366, 1:14)

# MCP percentile 
# E.g., 100% = all locations are included in creating the MCP
# 95% = all locations within the 95th percentile distance from the geographic centre
#       are included in creating the MCP
MCP_pctl <- 0.95

# dBBMM params
# dBBMM_ud - utilization distr isopleth to extract
#   E.g., 100% = full utilization distribution extracted for dBBMM
#   95% = 95th percentile isopleth extracted for dBBMM
# szn_window / szn_margin
#   dBBMM algorithm window & margin params for seasonal scale dBBMMs
# weekly_window / weekly_margin
#   dBBMM algorithm window & margin params for weekly scale dBBMMs
# TODO: would def be more efficient to save the entire raster stack of 
# calculated dBBMMs, then simply extract the isopleth of interest, so
# don't need to recalc the time consuming process of dBBMMs...
# do this if there's going to be lots of exploration of various UD levels.
# This comes at the cost of storage space - lots of rasters = lots of space.
dBBMM_ud <- 0.99
szn_window <- 57
szn_margin <- 9 
weekly_window <- 7
weekly_margin <- 3

#### PIPELINE ####
list(
  #### SETUP ####
  # Pull and track all collar keys files
  tar_target(collar_keys, collar::get_paths("data/Collar Keys"), format = "file"),
  # Download off Vectronix website
  tar_target(raw_collar_data, load_collar_data(collar_keys)),
  # Pull and track capture data file
  tar_target(capture_data_path, "data/DATABASE_Elk Capture_Updtd_20250415.xlsx", format = "file"),
  tar_target(capture_data, load_capture_data(capture_data_path)),
  # Assign capture ID to collar data
  tar_target(full_collar_data, attribute_animal_id(raw_collar_data, capture_data)),
  tar_target(unassigned_detections, full_collar_data[is.na(full_collar_data$animal_id), ]),
  tar_target(collar_data, full_collar_data[!is.na(full_collar_data$animal_id), ]),
  # Clean collar data AND filter to cutoff date
  # TODO: rarefy points using SSM, or at this step? TBD.
  tar_target(cleaned_collar_data, clean_collar_data(collar_data)), # full dataset, cleaned of any spikes, but NOT rarified and NOT filtered to study time period
  tar_target(flagged_pts, collar_data[!(collar_data$idposition %in% cleaned_collar_data$idposition), ]),
  # Make our main `elk` df for further analysis
  tar_target(elk, cleaned_collar_data |>
               rarify_pts() |> # rarify the points down to 8 fixes per day for all elk (some elk transmit more)
               dplyr::filter(dttm < cutoff_date) |>
               assign_daily_seasons(seasons = list("winter" = winter, # defined toward the top of this document
                                                   "spring" = spring, # defined toward the top of this document
                                                   "summer" = summer), # defined toward the top of this document
                                    date_col = "dttm") |>
               sf::st_write("temp/Pipeline outputs/elk_positions.shp", append = FALSE)),
  #### SUMMARY STATS + PLOTS ####
  # Logger dotplot
  tar_target(elk_dotplot, logger_dotplot(elk)),
  # N elk online per year-month
  tar_target(elk_per_year_month, n_elk_per_year_month(elk)),
  tar_target(p_elk_per_year_month, elk_per_year_month_plot(elk)),
  # N elk online per month
  tar_target(elk_per_month, n_elk_per_month(elk)),
  tar_target(p_elk_per_month, elk_per_month_plot(elk)),
  # N detections per elk summary
  tar_target(dets_per_elk, n_dets_per_elk(elk)),
  tar_target(p_dets_per_elk, elk_dets_hist(elk)),
  # Fix success / detection efficiency
  tar_target(detection_efficiency, fix_rate(elk)),
  tar_target(p_efficiency, elk_fix_hist(detection_efficiency)),
  tar_render(daily_dets_plots,
             "reports/elk_dets_per_day.Rmd",
             output_file = "elk_dets_per_day.pdf",
             params = list(elk_data = elk)),
  # Net squared displacement
  tar_render(nsd_plots,
             "reports/elk_nsd.Rmd",
             output_file = "elk_nsd.pdf",
             params = list(elk_data = elk)),
  #### SEASONAL HOME RANGE ESTIMATES ####
  ##### Minimum Convex Polygons (MCPs) #####
  tar_target(winter_mcp, seasonal_mcp(elk = elk,
                                       season = winter,
                                       min_days = 0.9, # we want a sample size of a minimum of 90% days in the dataset covered
                                       percent = MCP_pctl) |> # 95% MCP - convex hull that encompasses 95th distance-from-centre percentile points. Defaults to Delaunay triangulation to find the center of the points.
               sf::st_write(paste0("temp/Pipeline outputs/MCP_Winter_", (MCP_pctl * 100), "pctl.shp"),
                            append = FALSE)),
  tar_target(spring_mcp, seasonal_mcp(elk = elk,
                                       season = spring,
                                       min_days = 0.9,
                                       percent = MCP_pctl) |>
               sf::st_write(paste0("temp/Pipeline outputs/MCP_Spring_", (MCP_pctl * 100), "pctl.shp"),
                            append = FALSE)),
  tar_target(summer_mcp, seasonal_mcp(elk = elk,
                                       season = summer,
                                       min_days = 0.9,
                                       percent = MCP_pctl) |>
               sf::st_write(paste0("temp/Pipeline outputs/MCP_Summer_", (MCP_pctl * 100), "pctl.shp"),
                            append = FALSE)),
  tar_target(all_seasons_mcp, dplyr::bind_rows(winter_mcp, spring_mcp, summer_mcp)),
  tar_target(mcp_seasonal_summary, summarize_area(all_seasons_mcp, group_by = "season")),
  # TODO: summary plots of MCP areas (currently stored in `dBBMM_MCP_summary_plots.R`)
  ##### Dynamic Brownian Bridge Movement Models #####
  tar_target(winter_dbbmm, seasonal_dbbmm(elk = elk,
                                          season = winter,
                                          min_days = 0.9,
                                          margin = szn_margin, # 9 points ~= about ~1 day margin
                                          window.size = szn_window, # 57 points / 8 points per day = window size of ~7 days long
                                          location.error = 11.5, # GPS error in meters. Vectronic documentation indicates GPS error is on average 8-15m.
                                          ud_percent = dBBMM_ud) |>
               sf::st_write(paste0("temp/Pipeline outputs/dBBMM_Winter_window", szn_window, "_margin", szn_margin, "_", (dBBMM_ud * 100), "ud.shp"),
                            append = FALSE)),
  tar_target(spring_dbbmm, seasonal_dbbmm(elk = elk,
                                          season = spring,
                                          min_days = 0.9,
                                          margin = szn_margin,
                                          window.size = szn_window,
                                          location.error = 11.5,
                                          ud_percent = dBBMM_ud) |>
               sf::st_write(paste0("temp/Pipeline outputs/dBBMM_Spring_window", szn_window, "_margin", szn_margin, "_", (dBBMM_ud * 100), "ud.shp"),
                            append = FALSE)),
  tar_target(summer_dbbmm, seasonal_dbbmm(elk = elk,
                                          season = summer,
                                          min_days = 0.9,
                                          margin = szn_margin,
                                          window.size = szn_window,
                                          location.error = 11.5,
                                          ud_percent = dBBMM_ud) |>
               sf::st_write(paste0("temp/Pipeline outputs/dBBMM_Summer_window", szn_window, "_margin", szn_margin, "_", (dBBMM_ud * 100), "ud.shp"),
                            append = FALSE)),
  tar_target(all_seasons_dbbmm, dplyr::bind_rows(winter_dbbmm, spring_dbbmm, summer_dbbmm)),
  tar_target(dbbmm_seasonal_summary, summarize_area(all_seasons_dbbmm, group_by = "season")),
  # TODO: summary plots of dBBMM areas (currently stored in `dBBMM_MCP_summary_plots.R`)
  #### WEEKLY HOME RANGE ESTIMATES ####
  ##### MCP #####
  tar_target(weekly_mcps, weekly_mcp(elk = elk,
                                     min_days = 1, # percentage of days - we want 100% of days
                                     min_dets_per_day = 7, # we also want at minimum 7 detections per day, otherwise that week of data is thrown out
                                     percent = MCP_pctl) |> # 95% MCP - convex hull that encompasses 95th distance-from-centre percentile points. Defaults to Delaunay triangulation to find the center of the points.
               sf::st_write(paste0("temp/Pipeline outputs/MCP_Weekly_", (MCP_pctl * 100), "pctl.shp"),
                            append = FALSE)
             ),
  tar_target(weekly_mcp_seasonal_summary, assign_weekly_seasons(weekly_shp = weekly_mcps,
                                                                seasons = list("winter" = winter, # defined toward the top of this document
                                                                               "spring" = spring, # defined toward the top of this document
                                                                               "summer" = summer) # defined toward the top of this document
                                                                ) |>
               summarize_area(group_by = c("season"))),
  ###### Severe Winter Period MCPs ######
  tar_target(weekly_mcp_swp_summary, weekly_mcps |>
               dplyr::filter(!animal_id %in% non_swp_elk) |>
               dplyr::filter(week %in% swp_weeks) |>
               dplyr::mutate(year = isoyear) |>
               dplyr::mutate(year = dplyr::if_else(week < 3, year-1, year)) |>
               dplyr::mutate(year = paste0(year, "-", year+1)) |>
               summarize_area(group_by = "year")),
  ##### dBBMM #####
  tar_target(weekly_dbbmms, weekly_dbbmm(elk = elk,
                                         min_days = 1,
                                         min_dets_per_day = 7,
                                         window.size = weekly_window, # 21 hours
                                         margin = weekly_margin, # 9 hours
                                         location.error = 11.5,
                                         ud_percent = dBBMM_ud) |>
               sf::st_write(paste0("temp/Pipeline outputs/dBBMM_Weekly_window", weekly_window, "_margin", weekly_margin, "_", (dBBMM_ud * 100), "ud.shp"),
                            append = FALSE)),
  tar_target(weekly_dbbmm_seasonal_summary, assign_weekly_seasons(weekly_shp = weekly_dbbmms,
                                                                  seasons = list("winter" = winter, # defined toward the top of this document
                                                                                 "spring" = spring, # defined toward the top of this document
                                                                                 "summer" = summer) # defined toward the top of this document
                                                                  ) |>
               summarize_area(group_by = c("season"))),
  ###### Severe Winter Period dBBMMs ######
  tar_target(weekly_dbbmm_swp_summary, weekly_dbbmms |>
               dplyr::filter(!animal_id %in% non_swp_elk) |>
               dplyr::filter(week %in% swp_weeks) |>
               dplyr::mutate(year = isoyear) |>
               dplyr::mutate(year = dplyr::if_else(week < 3, year-1, year)) |>
               dplyr::mutate(year = paste0(year, "-", year+1)) |>
               summarize_area(group_by = "year")),
  #### DAILY HOME RANGE ESTIMATES ####
  ##### MCP #####
  # Only doing MCPs for weekly estimates. dBBMM window/margin params are too
  # sensitive to lower sample sizes for daily home range estimates.
  tar_target(daily_mcps, daily_mcp(elk = elk,
                                    min_dets_per_day = 8, # Minimum 8 detections per day (100% fix rate)
                                    percent = MCP_pctl) |> # 95% MCP - convex hull that encompasses 95th distance-from-centre percentile points. Defaults to Delaunay triangulation to find the center of the points.
               sf::st_write(paste0("temp/Pipeline outputs/MCP_Daily_", (MCP_pctl * 100), "pctl.shp"),
                            append = FALSE)),
  tar_target(daily_mcp_seasonal_summary, assign_daily_seasons(daily_shp = daily_mcps,
                                                              seasons = list("winter" = winter, # defined toward the top of this document
                                                                             "spring" = spring, # defined toward the top of this document
                                                                             "summer" = summer), # defined toward the top of this document
                                                              date_col = "date") |>
               summarize_area(group_by = c("season"))),
  ###### Severe Winter Period daily MCPs ######
  tar_target(daily_mcp_swp_summary, daily_mcps |>
               dplyr::mutate(doy = lubridate::yday(date)) |>
               dplyr::filter(!animal_id %in% non_swp_elk) |>
               dplyr::filter(doy %in% swp_days) |>
               dplyr::mutate(year = dplyr::if_else(doy < 15, year-1, year)) |>
               dplyr::mutate(year = paste0(year, "-", year+1)) |>
               summarize_area(group_by = "year")),
  #### SEASONAL HOME RANGE OVERLAP ####
  ##### Winter to Spring #####
  tar_target(mcp_winter_spring_overlap, prct_overlap(shp_1 = winter_mcp,
                                                     shp_2 = spring_mcp,
                                                     shp_1_name = "winter",
                                                     shp_2_name = "spring")),
  tar_target(dbbmm_winter_spring_overlap, prct_overlap(shp_1 = winter_dbbmm,
                                                       shp_2 = spring_dbbmm,
                                                       shp_1_name = "winter",
                                                       shp_2_name = "spring")),
  #### YEARLY SEASONAL SITE FIDELITY ####
  ##### Year-to-year overlap within a season #####
  ###### MCP ######
  tar_target(mcp_winter_yearly_overlap, yearly_prct_overlap(shp = winter_mcp)),
  tar_target(mcp_spring_yearly_overlap, yearly_prct_overlap(shp = spring_mcp)),
  tar_target(mcp_summer_yearly_overlap, yearly_prct_overlap(shp = summer_mcp)),
  ###### dBBMM ######
  tar_target(dbbmm_winter_yearly_overlap, yearly_prct_overlap(shp = winter_dbbmm)),
  tar_target(dbbmm_spring_yearly_overlap, yearly_prct_overlap(shp = spring_dbbmm)),
  tar_target(dbbmm_summer_yearly_overlap, yearly_prct_overlap(shp = summer_dbbmm)),
  #### STEP LENGTHS ####
  # Step length data is calculated during the GPS cleanup
  tar_target(step_length_seasonal_summary, elk |>
               sf::st_drop_geometry() |>
               dplyr::group_by(season) |>
               dplyr::summarise(mean_step = mean(step, na.rm = TRUE),
                                sd_step = sd(step, na.rm = TRUE),
                                median_step = median(step, na.rm = TRUE),
                                mean_nsd = mean(NSD, na.rm = TRUE),
                                sd_nsd = sd(NSD, na.rm = TRUE),
                                median_nsd = median(NSD, na.rm = TRUE),
                                N = dplyr::n())),
  #### DEM ATTRIBUTES ####
  ##### Download and extract DEM attributes #####
  # Download the BC CDED 30km DEM tiles, then for each elk GPS point,
  # extract elevation, slope grade (%), slope aspect (degrees), and
  # roughness.
  tar_target(cded, query_cded(elk = elk, output_dir = "GIS/DEM"), format = "file"),
  tar_target(elk_dem, extract_dem(elk, cded_path = cded)),
  #### LiDAR ATTRIBUTES ####
  ##### Download and extract LiDAR attributes #####
  # Pull the LiDAR-derived data products off the W:/ drive onto local
  # machine + keep track of it if it changes on the server, then extract
  # the data from it (elevation, slope grade (%), canopy height, edge
  # category, edge distance).
  # Keep track of the W:/ drive LiDAR file
  # tar_target(uwr_lidar_gdb_path,
  #            "W:/wlap/nan/Workarea/Ecosystems_share/LiDAR/LiDAR_Project2020/Forsite_NOGO_UWR_Deliverables_Sept2021/UWR_Deliverables/uwr_intermediate_north_island.gdb",
  #            format = "file"),
  # Make a local copy of the W:/ drive LiDAR file (this will get re-downloaded
  # if the W:/ drive copy is ever updated/modified)
  tar_target(uwr_lidar_gdb_path, "W:/wlap/nan/Workarea/Ecosystems_share/LiDAR/LiDAR_Project2020/Forsite_NOGO_UWR_Deliverables_Sept2021/UWR_Deliverables/uwr_intermediate_north_island.gdb"),
  tar_target(uwr_lidar_gdb,
             download_from_server(server_path = uwr_lidar_gdb_path,
                                  local_path = "GIS/LiDAR products",
                                  download = FALSE), # set to FALSE bc I just manually moved it over in the end
             format = "file"),
  tar_target(elk_uwr, extract_uwr(elk = elk,
                                    gdb = uwr_lidar_gdb,
                                    layers = c("canopy_height",
                                               "Edge_Category",
                                               "Edge_Distance_LiDAR",
                                               "elevation",
                                               "slope_percent")
                                    )),
  # Since the UWR layers might not be suitable for this analysis, let's
  # also extract data from a crown height model that was provided to us
  # by BCTS.
  tar_target(chm_path, "GIS/LiDAR products/crown_height.tif", format = "file"),
  tar_target(elk_chm, extract_chm(elk = elk,
                                  path = chm_path)),
  #### VRI ATTRIBUTES ####
  ##### Download and extract VRI attributes #####
  # Note we are using the improved VRI layer that was provided by Madrone.
  # tar_target(madrone_vri_gdb_path,
  #            "W:/wlap/nan/Workarea/Ecosystems_share/WHR_Models/2023/SEPT2023_v4/SEPT2023_v4_Elk Models and Spatial/Spatial/Operational_Data_6636.gdb",
  #            format = "file"),
  tar_target(madrone_vri_gdb_path, "W:/wlap/nan/Workarea/Ecosystems_share/WHR_Models/2023/SEPT2023_v4/SEPT2023_v4_Elk Models and Spatial/Spatial/Operational_Data_6636.gdb"),
  tar_target(madrone_vri_gdb, download_from_server(server_path = madrone_vri_gdb_path,
                                                   local_path = "GIS/VRI",
                                                   download = FALSE)), # set to FALSE bc I just manually moved it over in the end
  tar_target(vri, read_vri(gdb = madrone_vri_gdb)),
  #tar_target(vri_edges, extract_vri_edges(elk = elk, vri = vri)), # fails: not enough memory
  tar_target(elk_vri, extract_vri(feature = elk,
                                  vri = vri,
                                  cols = c("Shape_Area",
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
                                           "Creation_Date")))#,
  # tar_target(elk_edge_dist, st_edge_dist(feature = elk,
  #                                        edges = vri_edges))
)




